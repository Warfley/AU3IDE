unit au3Compiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Project, DOM, XMLRead, XMLWrite, AsyncProcess, process, strutils,
  Dialogs;

type
  TCompilerMode = (cmDebug, cmRelease);
  TOutputEvent = procedure(Sender: TObject; FileName: string; Output: string) of object;

  Tau3Compiler = class
  private
    FCurrentProject: Tau3Project;
    FCurrentMode: TCompilerMode;
    FOutput: TStringList;
    FCompilerRelease: string;
    FCProcess: TAsyncProcess;
    FCompilerDebug: string;
    FInterpreaterRelease: string;
    FInterpreaterDebug: string;
    FCompilerOutput: string;
    FInterpreaterOutput: string;
    FPrintCompilerOutput: boolean;
    FAdvancedCompilerOutput: boolean;
    FPrintInterpreaterOutput: boolean;
    FIsCompiling: boolean;
    FSTDOptions: TProcessOptions;
    FOnOutput: TOutputEvent;
    FOnFinishedRun: TNotifyEvent;
    FOnFinishedCompiling: TNotifyEvent;
    FOnCompileError: TNotifyEvent;
    function isRunning: boolean;
    procedure ReadData(Sender: TObject);
    procedure ProcTerm(Sender: TObject);
    procedure DoRun(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Stop;
    procedure CompileAndRun(P: Tau3Project; Mode: TCompilerMode);
    procedure ReadConf(Path: string);
    procedure WriteConf(Path: string);
    procedure Compile(P: Tau3Project; Mode: TCompilerMode);
    procedure Run(Path: string; Mode: TCompilerMode);

    property Active: boolean read isRunning;
    property CompilerReleasePath: string read FCompilerRelease write FCompilerRelease;
    property CompilerDebugPath: string read FCompilerDebug write FCompilerDebug;
    property InterpreterReleasePath: string
      read FInterpreaterRelease write FInterpreaterRelease;
    property InterpreterDebugPath: string read FInterpreaterDebug
      write FInterpreaterDebug;
    property InterpreterOutputPath: string read FInterpreaterOutput
      write FInterpreaterOutput;
    property CompilerOutputPath: string read FCompilerOutput write FCompilerOutput;
    property PrintCompilerOutput: boolean read FPrintCompilerOutput
      write FPrintCompilerOutput;
    property AdvancedCompilerOutput: boolean
      read FAdvancedCompilerOutput write FAdvancedCompilerOutput;
    property PrintInterpreaterOutput: boolean
      read FPrintInterpreaterOutput write FPrintInterpreaterOutput;
    property OnOutput: TOutputEvent read FOnOutput write FOnOutput;
    property OnFinishedRunning: TNotifyEvent read FOnFinishedRun write FOnFinishedRun;
    property OnFinishedCompiling: TNotifyEvent
      read FOnFinishedCompiling write FOnFinishedCompiling;
    property OnCompileError: TNotifyEvent read FOnCompileError write FOnCompileError;
  end;

implementation

procedure Tau3Compiler.ReadConf(Path: string);
var
  doc: TXMLDocument;
  tmpNode: TDOMNode;
begin
  try
    ReadXMLFile(doc, Path);
    tmpNode := doc.DocumentElement.FindNode('Compiler');
    FCompilerDebug := tmpNode.FindNode('Debug').TextContent;
    FCompilerRelease := tmpNode.FindNode('Release').TextContent;
    FCompilerOutput := tmpNode.FindNode('Output').TextContent;
    FPrintCompilerOutput := tmpNode.FindNode('PrintOutput').TextContent = 'True';
    FAdvancedCompilerOutput := tmpNode.FindNode('FullOutput').TextContent = 'True';
    tmpNode := doc.DocumentElement.FindNode('Interpret');
    FInterpreaterDebug := tmpNode.FindNode('Debug').TextContent;
    FInterpreaterRelease := tmpNode.FindNode('Release').TextContent;
    FInterpreaterOutput := tmpNode.FindNode('Output').TextContent;
    FPrintInterpreaterOutput := tmpNode.FindNode('PrintOutput').TextContent = 'True';
  finally
    doc.Free;
  end;
end;

procedure Tau3Compiler.WriteConf(Path: string);
var
  doc: TXMLDocument;
  tmpNode, tmp, rootnode: TDOMNode;
begin
  doc := TXMLDocument.Create;
  try
    rootnode := doc.CreateElement('CompilerConfig');
    doc.AppendChild(rootnode);
    tmpNode := doc.CreateElement('Compiler');
    rootnode.AppendChild(tmpNode);
    // Write Compiler Debug Path
    tmp := doc.CreateElement('Debug');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FCompilerDebug));
    // Write Compiler Release Path
    tmp := doc.CreateElement('Release');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FCompilerRelease));
    // Write Compiler Output Path
    tmp := doc.CreateElement('Output');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FCompilerOutput));
    // Write Compiler Printinfo
    tmp := doc.CreateElement('PrintOutput');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(BoolToStr(FPrintCompilerOutput, True)));
    // Write Compiler Fulloutput
    tmp := doc.CreateElement('FullOutput');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(BoolToStr(FAdvancedCompilerOutput, True)));

    tmpNode := doc.CreateElement('Interpret');
    rootnode.AppendChild(tmpNode);
    // Write Compiler Debug Path
    tmp := doc.CreateElement('Debug');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FInterpreaterDebug));
    // Write Compiler Release Path
    tmp := doc.CreateElement('Release');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FInterpreaterRelease));
    // Write Compiler Output Path
    tmp := doc.CreateElement('Output');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(FInterpreaterOutput));
    // Write Compiler Printinfo
    tmp := doc.CreateElement('PrintOutput');
    tmpNode.AppendChild(tmp);
    tmp.AppendChild(doc.CreateTextNode(BoolToStr(FPrintInterpreaterOutput, True)));
    WriteXML(doc, Path);
  finally
    doc.Free;
  end;
end;

procedure Tau3Compiler.Compile(P: Tau3Project; Mode: TCompilerMode);
begin
  Stop;
  FCurrentMode := Mode;
  FCurrentProject := P;
  FOutput.Clear;
  FIsCompiling := True;
  FCProcess.OnTerminate := @ProcTerm;
  FCProcess.Options := FSTDOptions + [poUsePipes, poStderrToOutPut{, poNoConsole}];
  if Mode = cmDebug then
    FCProcess.Executable := FCompilerDebug
  else
    FCProcess.Executable := FCompilerRelease;
  FCProcess.Parameters.Clear;
  FCProcess.Parameters.Add(P.MainFile);
  ForceDirectories(IncludeTrailingPathDelimiter(P.ProjectDir) + 'bin' + PathDelim);
  FCProcess.Parameters.Add(IncludeTrailingPathDelimiter(P.ProjectDir) +
    'bin' + PathDelim + 'out.bin');
  FCProcess.Parameters.Add(IncludeTrailingPathDelimiter(P.ProjectDir));
  FCProcess.Execute;
end;

procedure Tau3Compiler.Run(Path: string; Mode: TCompilerMode);
begin
  Stop;
  FCProcess.OnTerminate := @ProcTerm;
  FCProcess.Options := FSTDOptions + [poUsePipes, poStderrToOutPut];
  (*if FCurrentProject.GUIBased and (not FPrintInterpreaterOutput) then
    FCProcess.Options:=FCProcess.Options+[poNoConsole]
  else
    FCProcess.Options:=FCProcess.Options-[poNoConsole]; *)
  FCurrentMode := Mode;
  FIsCompiling := False;
  if mode = cmDebug then
    FCProcess.Executable := FInterpreaterDebug
  else
    FCProcess.Executable := FInterpreaterRelease;
  FOutput.Clear;
  FCProcess.Parameters.Clear;
  FCProcess.CurrentDirectory:=ExtractFilePath(Path);
  FCProcess.Execute;
end;

function Tau3Compiler.isRunning: boolean;
begin
  Result := FCProcess.Running;
end;

procedure Tau3Compiler.ReadData(Sender: TObject);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromStream(FCProcess.Output);
    FOutput.AddStrings(sl);
    for i := 0 to sl.Count - 1 do
      if (FIsCompiling and FPrintCompilerOutput) or (not FIsCompiling and FPrintInterpreaterOutput) then
      if AnsiContainsStr(sl[i], '[Line ') or AnsiContainsStr(sl[i], 'left:') or
        AnsiEndsStr('ms', sl[i]) or (not FIsCompiling) or (FAdvancedCompilerOutput) then
        if Assigned(FOnOutput) then
          FOnOutput(Self, FCurrentProject.MainFile, sl[i]);
  finally
    sl.Free;
  end;
end;

procedure Tau3Compiler.ProcTerm(Sender: TObject);
begin
  if FIsCompiling then
  begin
    ForceDirectories(ExtractFilePath(AnsiReplaceStr(FCompilerOutput,
      '($ProjDir)', ExcludeTrailingPathDelimiter(FCurrentProject.ProjectDir))));
    FOutput.SaveToFile(AnsiReplaceStr(FCompilerOutput, '($ProjDir)',
      ExcludeTrailingPathDelimiter(FCurrentProject.ProjectDir)));
  end
  else
  begin
    ForceDirectories(ExtractFilePath(AnsiReplaceStr(FInterpreaterOutput,
      '($ProjDir)', ExcludeTrailingPathDelimiter(FCurrentProject.ProjectDir))));
    FOutput.SaveToFile(AnsiReplaceStr(FInterpreaterOutput, '($ProjDir)',
      ExcludeTrailingPathDelimiter(FCurrentProject.ProjectDir)));
  end;
  if FIsCompiling and Assigned(FOnFinishedCompiling) then
    FOnFinishedCompiling(Self)
  else if not FIsCompiling and Assigned(FOnFinishedRun) then
    FOnFinishedRun(Self);
end;

procedure Tau3Compiler.DoRun(Sender: TObject);
begin
  ForceDirectories(ExtractFilePath(AnsiReplaceStr(FCompilerOutput,
    '($ProjDir)', ExcludeTrailingPathDelimiter(FCurrentProject.ProjectDir))));
  FOutput.SaveToFile(AnsiReplaceStr(FCompilerOutput, '($ProjDir)',
    ExcludeTrailingPathDelimiter(FCurrentProject.ProjectDir)));
  if AnsiContainsStr(FOutput.Text, '[Line') then
  begin
    if Assigned(FOnCompileError) then
      FOnCompileError(Self);
  end
  else
  Run(IncludeTrailingPathDelimiter(FCurrentProject.ProjectDir) +
    'bin' + PathDelim, FCurrentMode);
end;

constructor Tau3Compiler.Create;
begin
  FCompilerOutput:='($ProjDir)\Output\Compile.txt';
  FInterpreaterOutput:='($ProjDir)\Output\Interpreter.txt';
  FPrintCompilerOutput:=True;
  FCProcess := TAsyncProcess.Create(nil);
  FOutput := TStringList.Create;
  FCProcess.OnReadData := @ReadData;
  FSTDOptions:=FCProcess.Options;
end;

destructor Tau3Compiler.Destroy;
begin
  FCProcess.Free;
  FOutput.Free;
  inherited;
end;

procedure Tau3Compiler.Stop;
begin
  if FCProcess.Running then
    FCProcess.Terminate(-1);
end;

procedure Tau3Compiler.CompileAndRun(P: Tau3Project; Mode: TCompilerMode);
begin
  Stop;
  FCurrentMode := Mode;
  FCurrentProject := P;
  FOutput.Clear;
  FIsCompiling := True;
  FCProcess.OnTerminate := @DoRun;
  FCProcess.Options := FSTDOptions + [poUsePipes, poStderrToOutPut{, poNoConsole}];
  if Mode = cmDebug then
    FCProcess.Executable := FCompilerDebug
  else
    FCProcess.Executable := FCompilerRelease;
  FCProcess.Parameters.Clear;
  FCProcess.Parameters.Add(P.MainFile);
  ForceDirectories(IncludeTrailingPathDelimiter(P.ProjectDir) + 'bin' + PathDelim);
  FCProcess.Parameters.Add(IncludeTrailingPathDelimiter(P.ProjectDir) +
    'bin' + PathDelim + 'out.au3.bin');
  FCProcess.Parameters.Add(IncludeTrailingPathDelimiter(P.ProjectDir));
  FCProcess.Execute;
end;

end.
