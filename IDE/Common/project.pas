unit Project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, LazFileUtils, FileUtil, Dialogs, au3Types, ListRecords;

type
  EWrongVersion = Exception;

  TCompressionMode = (cmNone=0, cmLow=1, cmNormal=2, cmHigh=3, cmMax=4);
  TCompOptions = record
    Compression: TCompressionMode;
    IconPath: String;
    PackUPX: Boolean;
    OutPath: String;
  end;
  TVersion = record
    UseVersion, IncreaseBuilt: Boolean;
    Version, Subversion, Revision, Built: Integer;
  end;

  TAppType = (atConsole=0, atNoConsole=1, atGUI=2);

  Tau3Project = class
  private
    FPaths: TStringList;
    FFiles: TStringList;
    FMainFile: string;
    FProjectDir: string;
    FChanged: boolean;
    FName: string;
    FCompOptions: TCompOptions;
    FAppType: TAppType;
    FMainForm: string;
    FRunParams: TStringList;
    FFocusedFile: integer;
    FOpendFiles: TOpendFileList;
    FVersionData: TStringList;
    FVersion: TVersion;
    FOnChange: TNotifyEvent;
    FCheckInclude: TCheckIncludeEvent;
    FAddInclude: TAddIncludeEvent;
    procedure SetPaths(s:TStringList);
    procedure SetVersionData(s:TStringList);
    procedure SetMainFile(f: string);
    procedure SetCompOptions(c: TCompOptions);
    function GetMainFile: string;
    procedure SetMainForm(s: string);
    procedure SetOpendFiles(s: TOpendFileList);
    procedure SetProjectDir(p: string);
    function GetAbsoluteFileName(i: integer): string;
    procedure SetAbsoluteFileName(i: integer; f: string);
    procedure FilesChange(Sender: TObject);
    procedure SetRunParams(p: TStringList);
  public
    function GetAbsPath(Rel: string): string;
    function GetRelPath(Rel: string): string;
    function GetMainFileRel: string;
    function AddFile(F: string): integer;
    procedure DeleteFile(f: string);
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    procedure Clear;
    procedure ReadFromFile(f: string);
    procedure WriteToFile(f: string);
    property MainFile: string read GetMainFile write SetMainFile;
    property FilePath[i: integer]: string read GetAbsoluteFileName
      write SetAbsoluteFileName;
    property CompilerOptions: TCompOptions read FCompOptions write SetCompOptions;
    property Files: TStringList read FFiles;
    property ProjectDir: string read FProjectDir write SetProjectDir;
    property Changed: boolean read FChanged write FChanged;
    property Name: string read FName write FName;
    property AppType: TAppType read FAppType write FAppType;
    property MainForm: string read FMainForm write SetMainForm;
    property OpendFiles: TOpendFileList read FOpendFiles write SetOpendFiles;
    property FocusedFile: integer read FFocusedFile write FFocusedFile;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property CheckInclude: TCheckIncludeEvent read FCheckInclude write FCheckInclude;
    property AddInclude: TAddIncludeEvent read FAddInclude write FAddInclude;
    property RunParams: TStringList read FRunParams write SetRunParams;
    property Paths: TStringList read FPaths write SetPaths;
    property Version: TVersion read FVersion write FVersion;
    property VersionData: TStringList read FVersionData write SetVersionData;
  end;

const ProjectVersion = '1';

implementation

    procedure Tau3Project.SetRunParams(p: TStringList);
    begin
  FChanged := True;
      FRunParams.Assign(p);
  if Assigned(FOnChange) then
    FOnChange(Self);
    end;

    procedure Tau3Project.SetCompOptions(c: TCompOptions);
    begin
  FChanged := True;
      FCompOptions:=c;
  if Assigned(FOnChange) then
    FOnChange(Self);
    end;

procedure Tau3Project.SetProjectDir(p: string);
var
  i: integer;
  tmp: string;
begin
  tmp := GetMainFile;
  FMainFile := CreateRelativePath(tmp, p, True);
  for i := 0 to FFiles.Count - 1 do
  begin
    tmp := GetAbsoluteFileName(i);
    FFiles[i] := CreateRelativePath(tmp, p, True);
  end;
  FProjectDir := p;
  FChanged := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure Tau3Project.FilesChange(Sender: TObject);
begin
  FChanged := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure Tau3Project.SetPaths(s:TStringList);
begin
  FPaths.Assign(s);
  if Assigned(FOnChange) then
  FOnChange(Self);
end;

procedure Tau3Project.SetVersionData(s:TStringList);
begin
  FVersionData.Assign(s);
  if Assigned(FOnChange) then
  FOnChange(Self);
end;

procedure Tau3Project.SetMainForm(s: string);
begin
  FChanged := True;
  if FilenameIsAbsolute(s) then
    s := CreateRelativePath(s, FProjectDir, True);
  FMainForm := s;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure Tau3Project.SetOpendFiles(s: TOpendFileList);
begin
  FOpendFiles.Assign(s);
  if Assigned(FOnChange) then
  FOnChange(Self);
end;

procedure Tau3Project.SetMainFile(f: string);
begin
  FChanged := True;
  if FilenameIsAbsolute(F) then
    F := CreateRelativePath(F, FProjectDir, True);
  FMainFile := F;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function Tau3Project.GetMainFile: string;
begin
  if FilenameIsAbsolute(FMainFile) then
    Result := FMainFile
  else
    Result := CreateAbsoluteSearchPath(FMainFile, FProjectDir);
end;

function Tau3Project.GetAbsoluteFileName(i: integer): string;
var
  P: string;
begin
  P := FFiles[i];
  if FilenameIsAbsolute(P) then
    Result := P
  else
    Result := CreateAbsoluteSearchPath(P, FProjectDir);
end;

procedure Tau3Project.SetAbsoluteFileName(i: integer; f: string);
begin
  if FilenameIsAbsolute(F) then
    F := CreateRelativePath(F, FProjectDir, True);
  FFiles[i] := F;
end;

function Tau3Project.GetAbsPath(Rel: string): string;
begin
  if not FilenameIsAbsolute(Rel) then
    Rel := CreateAbsoluteSearchPath(Rel, FProjectDir);
  Result := Rel;
end;

function Tau3Project.GetRelPath(Rel: string): string;
begin
  if FilenameIsAbsolute(Rel) then
    Rel := CreateRelativePath(Rel, FProjectDir, True);
  Result := Rel;
end;

function Tau3Project.AddFile(F: string): integer;
begin
  if FilenameIsAbsolute(F) then
    F := CreateRelativePath(F, FProjectDir, True);
  Result := FFiles.Add(F);
end;

function Tau3Project.GetMainFileRel: string;
begin
  Result := FMainFile;
end;

procedure Tau3Project.Clear;
begin
  FFiles.Clear;
  FMainFile := '';
  FName := '';
  FProjectDir := '';
  FChanged := False;
  FOpendFiles.Clear;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure Tau3Project.DeleteFile(f: string);
var
  i: integer;
begin
  if FilenameIsAbsolute(f) then
    f := CreateRelativePath(f, FProjectDir, True);
  if f <> FMainFile then
    for i := 0 to FFiles.Count - 1 do
      if FFiles[i] = f then
      begin
        FFiles.Delete(i);
        Break;
      end;
end;

constructor Tau3Project.Create;
begin
  FFiles := TStringList.Create;
  FFiles.OnChange := @FilesChange;
  FOpendFiles := TOpendFileList.Create;
  FPaths:=TStringList.Create;
  FPaths.OnChange := @FilesChange;
  FRunParams:=TStringList.Create;
  FRunParams.OnChange :=@FilesChange;
  FVersionData:=TStringList.Create;
  FVersionData.OnChange:=@FilesChange;
  FillChar(FVersion, SizeOf(FVersion), #00);
end;

destructor Tau3Project.Destroy;
begin
  FFiles.Free;
  FOpendFiles.Free;
  FPaths.Free;
  FRunParams.Free;
  FVersionData.Free;
  inherited;
end;

procedure Tau3Project.Load;

  procedure GetInt(s1, s2: string; out l, p: integer);
  begin
    if not TryStrToInt(s1, l) then
      l := 1;
    if not TryStrToInt(s2, p) then
      p := 1;
  end;

var
  ProjFile: TXMLDocument;
  FilesNode: TDOMNode;
  i: integer;
  s1, s2: string;
  p, l: integer;
begin
  try
    FFiles.Clear;
    ReadXMLFile(ProjFile, IncludeTrailingPathDelimiter(FProjectDir) +
      FName + '.au3proj');
    if not ProjFile.DocumentElement.hasAttribute('Version') or (ProjFile.DocumentElement.Attributes.Item[0].TextContent<>ProjectVersion) then
      raise EWrongVersion.Create('Wrong Project Version');
    FMainFile := ProjFile.DocumentElement.FindNode('MainFile').TextContent;
    s1 := ProjFile.DocumentElement.FindNode('Apptype').TextContent;
    if s1 = 'GUI' then
      FAppType:=atGUI
    else if s1 = 'NoConsole' then FAppType:=atNoConsole else FAppType:=atConsole;
    if FAppType=atGUI then
      FMainForm := ProjFile.DocumentElement.FindNode('MainForm').TextContent;
    // Load Compiler Options
    FilesNode := ProjFile.DocumentElement.FindNode('CompilerOptions');
    FCompOptions.Compression:=TCompressionMode(StrToInt(FilesNode.FindNode('Compression').TextContent));
    FCompOptions.IconPath:=Trim(FilesNode.FindNode('Icon').TextContent);
    FCompOptions.OutPath:=FilesNode.FindNode('OutPath').TextContent;
    FCompOptions.PackUPX:=FilesNode.FindNode('UPX').TextContent='True';
    // Load Params
    FilesNode := ProjFile.DocumentElement.FindNode('RunParams');
    FRunParams.BeginUpdate;
    try
      FRunParams.Clear;
      for i := 0 to FilesNode.ChildNodes.Count - 1 do
        if FilesNode.ChildNodes.Item[i].NodeName = 'Param' then
          FRunParams.Add(FilesNode.ChildNodes.Item[i].TextContent);
    finally
    FRunParams.EndUpdate;
    end;
    // Load Version
    FilesNode := ProjFile.DocumentElement.FindNode('Version');
    FVersion.UseVersion:=FilesNode.FindNode('UseVersion').TextContent='True';
    FVersion.IncreaseBuilt:=FilesNode.FindNode('IncreaseBuilt').TextContent='True';
    FVersion.Version:=StrToInt(FilesNode.FindNode('Version').TextContent);
    FVersion.Subversion:=StrToInt(FilesNode.FindNode('Subversion').TextContent);
    FVersion.Revision:=StrToInt(FilesNode.FindNode('Revision').TextContent);
    FVersion.Built:=StrToInt(FilesNode.FindNode('Built').TextContent);
    // Load Version Data
    FilesNode := ProjFile.DocumentElement.FindNode('VersionData');
    FVersionData.BeginUpdate;
    try
      FVersionData.Clear;
      for i := 0 to FilesNode.ChildNodes.Count - 1 do
          FVersionData.Values[FilesNode.ChildNodes.Item[i].NodeName] := FilesNode.ChildNodes.Item[i].TextContent;
    finally
    FVersionData.EndUpdate;
    end;
    // Load Files
    FilesNode := ProjFile.DocumentElement.FindNode('Files');
    FFiles.BeginUpdate;
    try
      FFiles.Clear;
      for i := 0 to FilesNode.ChildNodes.Count - 1 do
        if FilesNode.ChildNodes.Item[i].NodeName = 'File' then
          FFiles.Add(FilesNode.ChildNodes.Item[i].TextContent);
    finally
      FFiles.EndUpdate;
    end;
    // Load Paths
    FilesNode := ProjFile.DocumentElement.FindNode('Paths');
    FPaths.BeginUpdate;
    try
      FPaths.Clear;
      for i := 0 to FilesNode.ChildNodes.Count - 1 do
        if FilesNode.ChildNodes.Item[i].NodeName = 'Path' then
          FPaths.Add(FilesNode.ChildNodes.Item[i].TextContent);
    finally
      FPaths.EndUpdate;
    end;
    FilesNode := ProjFile.DocumentElement.FindNode('OpendFiles');
    FFocusedFile := 0;
    for i := 0 to FilesNode.ChildNodes.Count - 1 do
      if FilesNode.ChildNodes.Item[i].NodeName = 'Opend' then
        with TDOMElement(FilesNode.ChildNodes.Item[i]) do
        begin
          s1 := GetAttribute('Line');
          s2 := GetAttribute('Pos');
          GetInt(s1, s2, l, p);
          if GetAttribute('Focused') = '1' then
            FFocusedFile := FOpendFiles.Add(OpendFileInfo(TextContent, l, p))
          else
            FOpendFiles.Add(OpendFileInfo(TextContent, l, p));
        end;
  finally
    ProjFile.Free;
  end;
  FChanged := False;
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure Tau3Project.Save;
var
  ProjFile: TXMLDocument;
  FilesNode, tmp, t: TDOMNode;
  i: integer;
  s: string;
begin
  ProjFile := TXMLDocument.Create;
  try
    tmp := ProjFile.CreateElement(FName);
    TDOMElement(tmp).SetAttribute('Version', ProjectVersion);
    ProjFile.AppendChild(tmp);
    // Create Mainfile Node
    tmp := ProjFile.CreateElement('MainFile');
    ProjFile.DocumentElement.AppendChild(tmp);
    t := ProjFile.CreateTextNode(FMainFile);
    tmp.AppendChild(t);
    // Create GUI Node
    tmp := ProjFile.CreateElement('Apptype');
    ProjFile.DocumentElement.AppendChild(tmp);
    case FAppType of
    atGUI: s:='GUI';
    atConsole: s:='Console';
    atNoConsole: s:='NoConsole';
    end;
    t := ProjFile.CreateTextNode(s);
    tmp.AppendChild(t);
    // Create MainForm Node
    if FAppType=atGUI then
    begin
      tmp := ProjFile.CreateElement('MainForm');
      ProjFile.DocumentElement.AppendChild(tmp);
      t := ProjFile.CreateTextNode(FMainForm);
      tmp.AppendChild(t);
    end;
    // Createing ParamNode
    FilesNode:=ProjFile.CreateElement('RunParams');
    ProjFile.DocumentElement.AppendChild(FilesNode);
    if FRunParams.Count = 0 then
      FilesNode.AppendChild(ProjFile.CreateTextNode(' '));
    for i := 0 to FRunParams.Count - 1 do
    begin
      tmp := ProjFile.CreateElement('Param');
      FilesNode.AppendChild(tmp);
      t := ProjFile.CreateTextNode(FRunParams[i]);
      tmp.AppendChild(t);
    end;
    // Create Compiler Options Node
    FilesNode:=ProjFile.CreateElement('CompilerOptions');
    ProjFile.DocumentElement.AppendChild(FilesNode);
    tmp:=ProjFile.CreateElement('Compression');
    FilesNode.AppendChild(tmp);
    tmp.AppendChild(ProjFile.CreateTextNode(IntToStr(ord(FCompOptions.Compression))));

    tmp:=ProjFile.CreateElement('UPX');
    FilesNode.AppendChild(tmp);
    tmp.AppendChild(ProjFile.CreateTextNode(BoolToStr(FCompOptions.PackUPX, 'True', 'False')));

    tmp:=ProjFile.CreateElement('OutPath');
    FilesNode.AppendChild(tmp);
    tmp.AppendChild(ProjFile.CreateTextNode(FCompOptions.OutPath));

    tmp:=ProjFile.CreateElement('Icon');
    FilesNode.AppendChild(tmp);
    tmp.AppendChild(ProjFile.CreateTextNode(FCompOptions.IconPath));
    // Creating Version Nodes
    FilesNode:=ProjFile.CreateElement('Version');
    ProjFile.DocumentElement.AppendChild(FilesNode);

    tmp:=ProjFile.CreateElement('UseVersion');
    FilesNode.AppendChild(tmp);
    tmp.AppendChild(ProjFile.CreateTextNode(BoolToStr(FVersion.UseVersion, 'True', 'False')));

    tmp:=ProjFile.CreateElement('IncreaseBuilt');
    FilesNode.AppendChild(tmp);
    tmp.AppendChild(ProjFile.CreateTextNode(BoolToStr(FVersion.UseVersion, 'True', 'False')));

    tmp:=ProjFile.CreateElement('Version');
    FilesNode.AppendChild(tmp);
    tmp.AppendChild(ProjFile.CreateTextNode(IntToStr(FVersion.Version)));

    tmp:=ProjFile.CreateElement('Subversion');
    FilesNode.AppendChild(tmp);
    tmp.AppendChild(ProjFile.CreateTextNode(IntToStr(FVersion.Subversion)));

    tmp:=ProjFile.CreateElement('Revision');
    FilesNode.AppendChild(tmp);
    tmp.AppendChild(ProjFile.CreateTextNode(IntToStr(FVersion.Revision)));

    tmp:=ProjFile.CreateElement('Built');
    FilesNode.AppendChild(tmp);
    tmp.AppendChild(ProjFile.CreateTextNode(IntToStr(FVersion.Built)));

    // Creating VersionData Nodes
    FilesNode:=ProjFile.CreateElement('VersionData');
    ProjFile.DocumentElement.AppendChild(FilesNode);
    for i:=0 to FVersionData.Count-1 do
    begin
      tmp:=ProjFile.CreateElement(FVersionData.Names[i]);
      FilesNode.AppendChild(tmp);
      tmp.AppendChild(ProjFile.CreateTextNode(FVersionData.ValueFromIndex[i]));
    end;

    // Createing file Nodes
    FilesNode := ProjFile.CreateElement('Files');
    ProjFile.DocumentElement.AppendChild(FilesNode);
    if FFiles.Count = 0 then
      FilesNode.AppendChild(ProjFile.CreateTextNode(' '));
    for i := 0 to FFiles.Count - 1 do
    begin
      tmp := ProjFile.CreateElement('File');
      FilesNode.AppendChild(tmp);
      t := ProjFile.CreateTextNode(FFiles[i]);
      tmp.AppendChild(t);
    end;
    // Createing Path Nodes
    FilesNode := ProjFile.CreateElement('Paths');
    ProjFile.DocumentElement.AppendChild(FilesNode);
    if FPaths.Count = 0 then
      FilesNode.AppendChild(ProjFile.CreateTextNode(' '));
    for i := 0 to FPaths.Count - 1 do
    begin
      tmp := ProjFile.CreateElement('File');
      FilesNode.AppendChild(tmp);
      t := ProjFile.CreateTextNode(FPaths[i]);
      tmp.AppendChild(t);
    end;
    // Createing OpendFile Nodes
    FilesNode := ProjFile.CreateElement('OpendFiles');
    ProjFile.DocumentElement.AppendChild(FilesNode);
    if FOpendFiles.Count = 0 then
      FilesNode.AppendChild(ProjFile.CreateTextNode(' '));
    for i := 0 to FOpendFiles.Count - 1 do
    begin
      tmp := ProjFile.CreateElement('Opend');
      if i = FFocusedFile then
        TDOMElement(tmp).SetAttribute('Focused', '1');
      TDOMElement(tmp).SetAttribute('Line', IntToStr(FOpendFiles[i].Line));
      TDOMElement(tmp).SetAttribute('Pos', IntToStr(FOpendFiles[i].Pos));
      FilesNode.AppendChild(tmp);
      t := ProjFile.CreateTextNode(FOpendFiles[i].Name);
      tmp.AppendChild(t);
    end;
    // Write To File
    WriteXMLFile(ProjFile, IncludeTrailingPathDelimiter(FProjectDir) +
      FName + '.au3proj');
  finally
    ProjFile.Free;
  end;
  // Add support for Forms here
  if Assigned(FCheckInclude) and Assigned(FAddInclude) and (AppType=atGUI) then
    if not FCheckInclude(MainFile, ChangeFileExt(MainForm, '.au3')) then
      FAddInclude(MainFile, ChangeFileExt(MainForm, '.au3'));
  FChanged := False;
end;

procedure Tau3Project.ReadFromFile(f: string);
begin
  SetProjectDir(ExtractFilePath(f));
  FName := ExtractFileName(ExtractFileNameWithoutExt(f));
  Load;
end;

procedure Tau3Project.WriteToFile(f: string);
begin
  SetProjectDir(ExtractFilePath(f));
  FName := ExtractFileName(ExtractFileNameWithoutExt(f));
  FCompOptions.OutPath:=Format('bin/%s.exe', [FName]);
  Save;
  if Assigned(FOnChange) then
    FOnChange(self);
end;

end.
