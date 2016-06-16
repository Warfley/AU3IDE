unit Project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, LazFileUtils, FileUtil, Dialogs, au3Types, ListRecords;

type
  Tau3Project = class
  private
    FFiles: TStringList;
    FMainFile: string;
    FProjectDir: string;
    FChanged: boolean;
    FName: string;
    FGUIBased: boolean;
    FMainForm: string;
    FFocusedFile: integer;
    FOpendFiles: TOpendFileList;
    FOnChange: TNotifyEvent;
    FCheckInclude: TCheckIncludeEvent;
    FAddInclude: TAddIncludeEvent;
    procedure SetMainFile(f: string);
    function GetMainFile: string;
    procedure SetMainForm(s: string);
    procedure SetOpendFiles(s: TOpendFileList);
    procedure SetProjectDir(p: string);
    function GetAbsoluteFileName(i: integer): string;
    procedure SetAbsoluteFileName(i: integer; f: string);
    procedure FilesChange(Sender: TObject);
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
    property Files: TStringList read FFiles;
    property ProjectDir: string read FProjectDir write SetProjectDir;
    property Changed: boolean read FChanged write FChanged;
    property Name: string read FName write FName;
    property GUIBased: boolean read FGUIBased write FGUIBased;
    property MainForm: string read FMainForm write SetMainForm;
    property OpendFiles: TOpendFileList read FOpendFiles write SetOpendFiles;
    property FocusedFile: integer read FFocusedFile write FFocusedFile;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property CheckInclude: TCheckIncludeEvent read FCheckInclude write FCheckInclude;
    property AddInclude: TAddIncludeEvent read FAddInclude write FAddInclude;
  end;

implementation

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
  FOpendFiles := TOpendFileList.Create;
  FFiles.OnChange := @FilesChange;
end;

destructor Tau3Project.Destroy;
begin
  FFiles.Free;
  FOpendFiles.Free;
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
    FMainFile := ProjFile.DocumentElement.FindNode('MainFile').TextContent;
    FGUIBased := ProjFile.DocumentElement.FindNode('Apptype').TextContent = 'GUI';
    if FGUIBased then
      FMainForm := ProjFile.DocumentElement.FindNode('MainForm').TextContent;
    FilesNode := ProjFile.DocumentElement.FindNode('Files');
    FFiles.BeginUpdate;
    try
      for i := 0 to FilesNode.ChildNodes.Count - 1 do
        if FilesNode.ChildNodes.Item[i].NodeName = 'File' then
          FFiles.Add(FilesNode.ChildNodes.Item[i].TextContent);
    finally
      FFiles.EndUpdate;
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
    ProjFile.AppendChild(tmp);
    // Create Mainfile Node
    tmp := ProjFile.CreateElement('MainFile');
    ProjFile.DocumentElement.AppendChild(tmp);
    t := ProjFile.CreateTextNode(FMainFile);
    tmp.AppendChild(t);
    // Create GUI Node
    tmp := ProjFile.CreateElement('Apptype');
    ProjFile.DocumentElement.AppendChild(tmp);
    if FGUIBased then
      s := 'GUI'
    else
      s := 'CONSOLE';
    t := ProjFile.CreateTextNode(s);
    tmp.AppendChild(t);
    // Create MainForm Node
    if FGUIBased then
    begin
      tmp := ProjFile.CreateElement('MainForm');
      ProjFile.DocumentElement.AppendChild(tmp);
      t := ProjFile.CreateTextNode(FMainForm);
      tmp.AppendChild(t);
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
  if Assigned(FCheckInclude) and Assigned(FAddInclude) and FGUIBased then
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
  Save;
  if Assigned(FOnChange) then
    FOnChange(self);
end;

end.
