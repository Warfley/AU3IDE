unit au3Compiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Project, DOM, XMLRead, XMLWrite, AsyncProcess, process, strutils,
  Dialogs, LazFileUtils;

type
  TCompileArch = (cax86, ca64);
  TOutputEvent = procedure(Sender: TObject; FileName: string; Output: string) of object;

  Tau3Compiler = class
  private
    FCurrentProject: Tau3Project;
    FArch: TCompileArch;
    FOutput: TStringList;
    FPath: string;
    FSaveIntData: boolean;
    FCProcess: TAsyncProcess;
    FIsCompiling: boolean;
    FSTDOptions: TProcessOptions;
    FOnOutput: TOutputEvent;
    FOnFinishedRun: TNotifyEvent;
    FOnFinishedCompiling: TNotifyEvent;
    FOnCompileError: TNotifyEvent;
    function isRunning: boolean;
    procedure ReadData(Sender: TObject);
    procedure ProcTerm(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Stop;
    procedure Run(P: Tau3Project; Arch: TCompileArch);
    procedure ReadConf(Path: string);
    procedure WriteConf(Path: string);
    procedure Compile(P: Tau3Project; Arch: TCompileArch);

    property Active: boolean read isRunning;
    property Path: string read FPath write FPath;
    property SaveIntData: boolean read FSaveIntData write FSaveIntData;
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
    tmpNode := doc.DocumentElement.FindNode('Path');
    FPath := tmpNode.TextContent;
    tmpNode := doc.DocumentElement.FindNode('SaveOutput');
    FSaveIntData:=tmpNode.TextContent='True';
    FSaveIntData := tmpNode.TextContent = 'True';
  finally
    doc.Free;
  end;
end;

procedure Tau3Compiler.WriteConf(Path: string);
var
  doc: TXMLDocument;
  tmpNode, rootnode: TDOMNode;
begin
  doc := TXMLDocument.Create;
  try
    rootnode := doc.CreateElement('CompilerConfig');
    doc.AppendChild(rootnode);
    tmpNode := doc.CreateElement('Path');
    rootnode.AppendChild(tmpNode);
    // Write Path
    tmpNode.AppendChild(doc.CreateTextNode(FPath));

    tmpNode := doc.CreateElement('SaveOutput');
    rootnode.AppendChild(tmpNode);
    // Write SaveData
    tmpNode.AppendChild(doc.CreateTextNode(IfThen(FSaveIntData, 'True', 'False')));

    WriteXML(doc, Path);
  finally
    doc.Free;
  end;
end;

procedure Tau3Compiler.Compile(P: Tau3Project; Arch: TCompileArch);
var v: TVersion;
  i: Integer;
begin
  Stop;
  FArch := Arch;
  FCurrentProject := P;
  FOutput.Clear;
  FIsCompiling := True;
  FCProcess.OnTerminate := @ProcTerm;
  FCProcess.Options := FSTDOptions + [poUsePipes, poStderrToOutPut];

  FCProcess.Executable := IncludeTrailingPathDelimiter(FPath) + 'Aut2Exe' +
    PathDelim + 'Aut2exe.exe';

  FCProcess.Parameters.Clear;
  FCProcess.Parameters.Add('/in');
  FCProcess.Parameters.Add(P.MainFile);
  if Length(FCurrentProject.CompilerOptions.OutPath)>0 then
  begin
  FCProcess.Parameters.Add('/out');
  FCProcess.Parameters.Add(CreateAbsoluteSearchPath(
    FCurrentProject.CompilerOptions.OutPath, FCurrentProject.ProjectDir));
  end;
  if Length(FCurrentProject.CompilerOptions.IconPath) > 0 then
  begin
    FCProcess.Parameters.Add('/icon');
    FCProcess.Parameters.Add(FCurrentProject.CompilerOptions.IconPath);
  end;
  FCProcess.Parameters.Add('/comp');
  FCProcess.Parameters.Add(IntToStr(Ord(FCurrentProject.CompilerOptions.Compression)));
  if FCurrentProject.CompilerOptions.PackUPX then
    FCProcess.Parameters.Add('/pack');
  if FCurrentProject.AppType<>atConsole then
    FCProcess.Parameters.Add('/gui')
  else
    FCProcess.Parameters.Add('/console');
  if Arch = cax86 then
    FCProcess.Parameters.Add('/x86')
  else
    FCProcess.Parameters.Add('/x64');

  if FCurrentProject.Version.IncreaseBuilt and FCurrentProject.Version.UseVersion then
  begin
    v:=FCurrentProject.Version;
    v.Built:=v.Built+1;
    FCurrentProject.Version:=v;
    FCurrentProject.VersionData.Values['FileVersion']:=
      Format('%d.%d.%d.%d', [v.Version, v.Subversion, v.Revision, v.Built]);
  end;

  for i:=0 to FCurrentProject.VersionData.Count-1 do
    if FCurrentProject.VersionData.ValueFromIndex[i]<>'' then
    begin
      FCProcess.Parameters.Add('/'+LowerCase(FCurrentProject.VersionData.Names[i]));
      FCProcess.Parameters.Add(Format('"%s"', [FCurrentProject.VersionData.ValueFromIndex[i]]));
    end;

  ForceDirectory(ExtractFilePath(CreateAbsoluteSearchPath(
    FCurrentProject.CompilerOptions.OutPath, FCurrentProject.ProjectDir)));

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
      if Assigned(FOnOutput) then
        FOnOutput(Self, FCurrentProject.MainFile, sl[i]);
  finally
    sl.Free;
  end;
end;

procedure Tau3Compiler.ProcTerm(Sender: TObject);
begin
  if not FIsCompiling and FSaveIntData then
    FOutput.SaveToFile(IncludeTrailingPathDelimiter(
      FCurrentProject.ProjectDir) + 'Run.log');
  if FIsCompiling and Assigned(FOnFinishedCompiling) then
    FOnFinishedCompiling(Self)
  else if not FIsCompiling and Assigned(FOnFinishedRun) then
    FOnFinishedRun(Self);
end;

constructor Tau3Compiler.Create;
begin
  FCProcess := TAsyncProcess.Create(nil);
  FOutput := TStringList.Create;
  FCProcess.OnReadData := @ReadData;
  FSTDOptions := FCProcess.Options;
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

procedure Tau3Compiler.Run(P: Tau3Project; Arch: TCompileArch);
begin
  Stop;
  FArch := Arch;
  FCurrentProject := P;
  FOutput.Clear;
  FIsCompiling := False;
  FCProcess.OnTerminate := @ProcTerm;
  FCProcess.Options := FSTDOptions + [poUsePipes, poStderrToOutPut];

  if Arch = cax86 then
  FCProcess.Executable := IncludeTrailingPathDelimiter(FPath) + 'AutoIt3.exe'
  else
  FCProcess.Executable := IncludeTrailingPathDelimiter(FPath) + 'AutoIt3_x64.exe';

  FCProcess.Parameters.Clear;
  FCProcess.Parameters.Add('/ErrorStdOut');
  FCProcess.Parameters.Add(P.MainFile);
  FCProcess.Parameters.AddStrings(FCurrentProject.RunParams);

  FCProcess.Execute;
end;

end.
