unit au3Types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ListRecords, Graphics, LazFileUtils, strutils, Dialogs,
  gstack;

type

  TPropertyChangeEvent = procedure(Sender: TObject;
    PropName, PropVal, OldVal: string) of object;

  TChangeData = record
    comp: TObject;
    Prop, OldVal, NewVal: string;
  end;

  TPropChangeStack = specialize TStack<TChangeData>;

  TTokenType = (tkComment, tkIdentifier, tkFunction, tkSymbol, tkNumber, tkSpace,
    tkString, tkUnknown, tkVar, tkUndefined, tkDoc, tkTemp);
  PHashInfo = ^THashInfo;

  TOpenEditorEvent = procedure(Filename: string; Pos: TPoint) of object;
  TCloseEditorEvent = procedure(Filename: string) of object;
  TCheckIncludeEvent = function(FileName, IncludeFile: string): boolean of object;
  TAddIncludeEvent = procedure(FileName, IncludeFile: string) of object;
  TChangeMainFormEvent = procedure(FileName: string; Silent: boolean = False) of object;

  TOpenFunctionEvent = function(FileName, FuncName: string; Params: string;
    CreateNew: boolean): string of object;

  TEditorConfig = packed record
    CERight: boolean;
    BGCol: TColor;
    EditBGCol: TColor;
    GutterCol: TColor;
    GutterFore: TColor;
    GutterEdited: TColor;
    GutterSaved: TColor;
    SelCol: TColor;
    SelFCol: TColor;
    TextColor: TColor;
    PastEOL: boolean;
    CaretAV: boolean;
    TabWidth: integer;
    TTipColor: TColor;
    TTipFont: TColor;
    EditorFont: TFontData;
    FontName: ShortString;
  end;

  TFormEditorConfig = packed record
    OIRight: boolean;
    BGCol: TColor;
    ForeCol: TColor;
    TBCol: TColor;
    UseHelpLines: boolean;
    UseRaster: boolean;
    RasterSize: integer;
    DoubleBuffer: boolean;
  end;

  THashInfo = record
    Key: ansistring;
    Kind: TTokenType;
  end;

  TSelectedItem = record
    Line, Pos: integer;
  end;

  TOpendFileList = specialize TFPGList<TOpendFileInfo>;
  TFuncList = specialize TFPGList<TFuncInfo>;
  TVarList = specialize TFPGList<TVarInfo>;

  TRangeType = (rtFunc, rtWhile, rtIf, rtFor);

  TDefRange = class
  private
    FStart, FEnd: integer;
    FVars: TVarList;
    FRangeType: TRangeType;
  public
    constructor Create;
    destructor Destroy; override;
    property Vars: TVarList read FVars;
    property StartLine: integer read FStart write FStart;
    property RangeType: TRangeType read FRangeType write FRangeType;
    property EndLine: integer read FEnd write FEnd;
  end;

const
  Version = '0.0.6';
  SUpdateURL = 'http://kehrein.org/AS/Updates/';

function OpendFileInfo(Name: string; Line: integer = 1;
  Pos: integer = 1; View: integer = 0): TOpendFileInfo;
function FuncInfo(Name: string; Line: integer; Inf: string = '';
  FName: string = ''): TFuncInfo;
function SelectedItem(Line, Pos: integer): TSelectedItem;
function VarInfo(Name: string; Line, Position: integer; FName: string = ''): TVarInfo;
function isEnd(s, endTok: string; ex: boolean = False): boolean;
function StringsContain(s: TStrings; str: string): boolean;
function GetFullPath(Filename, IncludePath, FPath: string; Paths: TStringList): string;
function GetRelInclude(FullPath, IncludePath, FPath: string;
  Paths: TStringList): string;
function ExtractBetween(const Value, A, B: string): string;

implementation


function ExtractBetween(const Value, A, B: string): string;
var
  aPos, bPos: integer;
begin
  Result := '';
  aPos := Pos(A, Value);
  if aPos > 0 then
  begin
    aPos := aPos + Length(A);
    bPos := PosEx(B, Value, aPos);
    if bPos > 0 then
    begin
      Result := Copy(Value, aPos, bPos - aPos);
    end;
  end;
end;

function StringsContain(s: TStrings; str: string): boolean;
var
  i: integer;
begin
  str := LowerCase(str);
  Result := False;
  for i := 0 to s.Count - 1 do
    if LowerCase(s[i]) = str then
    begin
      Result := True;
      exit;
    end;
end;


function isEnd(s, endTok: string; ex: boolean = False): boolean;

  function getFirstTok(str: string): string;
  var
    i, len: integer;
  begin
    len := 1;
    if length(str) > 1 then
    begin
      i := 2;
      if ex then
        while (str[i] in ['0'..'9', 'A'..'Z', 'a'..'z', '_', '-', '#', '.', ',']) do
        begin
          Inc(i);
          Inc(len);
        end
      else
        while (str[i] in ['0'..'9', 'A'..'Z', 'a'..'z', '_']) do
        begin
          Inc(i);
          Inc(len);
        end;
    end;
    Result := Copy(str, 1, len);
  end;

var
  l, l2: integer;
begin
  s := Trim(s);
  s := getFirstTok(s);
  l := Length(endTok);
  l2 := Length(s);
  Result := False;
  if l2 < l then
  begin
    Exit;
  end
  else if LowerCase(s) = LowerCase(endTok) then
  begin
    Result := True;
    Exit;
  end;
end;

function OpendFileInfo(Name: string; Line: integer = 1; Pos: integer = 1;
  View: integer = 0): TOpendFileInfo;
begin
  Result.Name := Name;
  Result.Line := Line;
  Result.Pos := Pos;
  Result.View := View;
end;

function SelectedItem(Line, Pos: integer): TSelectedItem;
begin
  Result.Line := Line;
  Result.Pos := Pos;
end;

function FuncInfo(Name: string; Line: integer; Inf: string = '';
  FName: string = ''): TFuncInfo;
begin
  Result.Name := Name;
  Result.Line := Line;
  Result.Info := Inf;
  Result.FileName := FName;
end;

function VarInfo(Name: string; Line, Position: integer; FName: string = ''): TVarInfo;
begin
  Result.Name := Name;
  Result.Line := Line;
  Result.Pos := Position;
  Result.FileName := FName;
end;

function GetFullPath(Filename, IncludePath, FPath: string; Paths: TStringList): string;
var
  p: string;
begin
  Result := CreateAbsoluteSearchPath(Filename, FPath);
  if not FileExistsUTF8(Result) then
  begin
    Result := CreateAbsoluteSearchPath(Filename, IncludePath);
    if not FileExistsUTF8(Result) then
    begin
      for p in Paths do
      begin
        Result := CreateAbsoluteSearchPath(Filename, p);
        if FileExistsUTF8(Result) then
          Exit;
      end;
      Result := '';
    end;
  end;
  // Ugly but working
  Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
end;

function GetRelInclude(FullPath, IncludePath, FPath: string;
  Paths: TStringList): string;
var
  p, p2: string;
begin
  Result := CreateRelativePath(FullPath, FPath, True);
  p := CreateRelativePath(FullPath, IncludePath, True);
  if Length(p) < Length(Result) then
    Result := p;
  for p2 in Paths do
  begin
    p := CreateRelativePath(FullPath, p2, True);
    if Length(p) < Length(Result) then
      Result := p;
  end;
end;

constructor TDefRange.Create;
begin
  FVars := TVarList.Create;
end;

destructor TDefRange.Destroy;
begin
  FVars.Free;
  inherited;
end;

end.
