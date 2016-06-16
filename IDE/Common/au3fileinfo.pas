unit au3FileInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, au3Types, ListRecords, contnrs, UnitParser;

type

  { Tau3File }

  Tau3File = class
  private
    FFileName: string;
    FRequired: TStringList;
    FFuncs: TFuncList;
    FVars: TVarList;

    procedure SetFuncs(f: TFuncList);
    procedure SetVars(v: TVarList);
    procedure SetReq(s: TStringList);

    procedure Parsed(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;

    property Functions: TFuncList read FFuncs write SetFuncs;
    property Variables: TVarList read FVars write SetVars;
    property RequiredFiles: TStringList read FRequired write SetReq;
    property FileName: string read FFileName write FFileName;
  end;

  { Tau3FileManager }

  Tau3FileManager = class
  private
    FFiles: TObjectList;
    function GetFile(i: integer): Tau3File;
    function CheckFileName(n: string): boolean;
    function GetFileIndex(Name: string): integer;
    procedure SetFileOpend(Name: string; Open: boolean);
    function GetCount: integer;
  public
    function CreateFile(FName: string): integer;
    function LoadFile(FName: string): integer;
    procedure UnloadFile(i: integer);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;

    property Files[i: integer]: Tau3File read GetFile; default;
    property FileOpend[s: string]: boolean read CheckFileName write SetFileOpend;
    property FileIndex[Name: string]: integer read GetFileIndex;
    property Count: integer read GetCount;
  end;

implementation

{ Tau3File }

procedure Tau3File.SetFuncs(f: TFuncList);
var
  i: integer;
begin
  FFuncs.Clear;
  for i := 0 to f.Count - 1 do
    FFuncs.Add(FuncInfo(f[i].Name, f[i].Line, f[i].Info, FFileName));
end;

procedure Tau3File.SetVars(v: TVarList);
var
  i: integer;
begin
  FVars.Clear;
  for i := 0 to v.Count - 1 do
    FVars.Add(VarInfo(v[i].Name, v[i].Line, v[i].Pos, FFileName));
end;

procedure Tau3File.SetReq(s: TStringList);
begin
  FRequired.Clear;
  FRequired.AddStrings(s);
end;

procedure Tau3File.Parsed(Sender: TObject);
var
  tmpFunc: TFuncList;
  tmpVar: TVarList;
begin
  tmpFunc := TFuncList.Create;
  try
    tmpFunc.Assign(FFuncs);
    SetFuncs(tmpFunc);
  finally
    tmpFunc.Free;
  end;
  tmpVar := TVarList.Create;
  try
    tmpVar.Assign(FVars);
    SetVars(tmpVar);
  finally
    tmpVar.Free;
  end;
end;

constructor Tau3File.Create;
begin
  FFuncs := TFuncList.Create;
  FVars := TVarList.Create;
  FRequired := TStringList.Create;
end;

destructor Tau3File.Destroy;
begin
  FFuncs.Free;
  FVars.Free;
  FRequired.Free;
end;

procedure Tau3File.Load;
var
  u: TUnitParser;
  sl: TStringList;
begin
  if not FileExists(FFileName) then
    Exit;
  u := TUnitParser.Create(True);
  u.FreeOnTerminate := True;
  u.OnFinished := @Parsed;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FFileName);
    u.Text := sl.Text;
  finally
    sl.Free;
  end;
  u.Funcs := FFuncs;
  u.Vars := FVars;
  u.RequiredFiles := FRequired;
  u.Ranges := nil;
  u.Start;
end;

{ Tau3FileManager }

function Tau3FileManager.GetFile(i: integer): Tau3File;
begin
  Result := FFiles[i] as Tau3File;
end;

function Tau3FileManager.CheckFileName(n: string): boolean;
begin
  Result := GetFileIndex(n) >= 0;
end;

function Tau3FileManager.GetFileIndex(Name: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FFiles.Count - 1 do
    if GetFile(i).FileName = Name then
    begin
      Result := i;
      Break;
    end;
end;

procedure Tau3FileManager.SetFileOpend(Name: string; Open: boolean);
begin
  if Open and not CheckFileName(Name) then
    LoadFile(Name)
  else if Open and CheckFileName(Name) then
    UnloadFile(GetFileIndex(Name));
end;

function Tau3FileManager.GetCount: integer;
begin
  Result := FFiles.Count;
end;

function Tau3FileManager.CreateFile(FName: string): integer;
begin
  if GetFileIndex(FName) >= 0 then
  begin
    Result := GetFileIndex(FName);
    exit;
  end;
  Result := FFiles.Add(Tau3File.Create);
  (FFiles[Result] as Tau3File).FileName := FName;
end;

function Tau3FileManager.LoadFile(FName: string): integer;
begin
  if GetFileIndex(FName) >= 0 then
  begin
    Result := GetFileIndex(FName);
    exit;
  end;
  Result := FFiles.Add(Tau3File.Create);
  with (FFiles[Result] as Tau3File) do
  begin
    FileName := FName;
    Load;
  end;
end;

procedure Tau3FileManager.UnloadFile(i: integer);
begin
  if (i >= 0) and (i < FFiles.Count) then
  begin
    FFiles[i].Free;
    FFiles.Delete(i);
  end;
end;

procedure Tau3FileManager.Clear;
begin
  while Count > 0 do
    UnloadFile(0);
end;

constructor Tau3FileManager.Create;
begin
  FFiles := TObjectList.Create(False);
end;

destructor Tau3FileManager.Destroy;
begin
  Clear;
  FFiles.Free;
end;

end.
