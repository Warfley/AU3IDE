unit au3Types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ListRecords, Graphics;

type
  TTokenType = (tkComment, tkIdentifier, tkFunction, tkSymbol, tkNumber, tkSpace,
    tkString, tkUnknown, tkVar, tkUndefined, tkDoc, tkTemp);
  PHashInfo = ^THashInfo;

  TOpenEditorEvent = procedure(Filename: string; Pos: TPoint) of object;
  TCloseEditorEvent = procedure(Filename: String) of object;
  TCheckIncludeEvent = function(FileName, IncludeFile: String): Boolean of object;
  TAddIncludeEvent = procedure(FileName, IncludeFile: String) of object;
  TChangeMainFormEvent = procedure(FileName: String) of Object;

  TOpenFunctionEvent = function(FileName, FuncName: string; Params: String;
    CreateNew: Boolean): string of object;

  TEditorConfig = packed record
    CERight: Boolean;
    BGCol: TColor;
    EditBGCol: TColor;
    GutterCol: TColor;
    GutterFore: TColor;
    GutterEdited: TColor;
    GutterSaved: TColor;
    SelCol: TColor;
    SelFCol: TColor;
    TextColor: TColor;
    PastEOL: Boolean;
    CaretAV: Boolean;
    TabWidth: Integer;
    TTipColor: TColor;
    TTipFont: TColor;
    EditorFont: TFontData;
    FontName: ShortString;
  end;

  TFormEditorConfig = packed record
    OIRight: Boolean;
    BGCol: TColor;
    ForeCol: TColor;
    TBCol: TColor;
    UseHelpLines: Boolean;
    UseRaster: Boolean;
    RasterSize: Integer;
    DoubleBuffer: Boolean;
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
  Version = '0.0.5';
  SUpdateURL = 'http://kehrein.org/AET/Updates/';

function OpendFileInfo(Name: string; Line: integer = 1;
  Pos: integer = 1): TOpendFileInfo;
function FuncInfo(Name: string; Line: integer; Inf: string = '';
  FName: string = ''): TFuncInfo;
function SelectedItem(Line, Pos: integer): TSelectedItem;
function VarInfo(Name: string; Line, Position: integer; FName: string = ''): TVarInfo;
function isEnd(s, endTok: string; ex: Boolean=false): boolean;
function StringsContain(s: TStrings; str: String): Boolean;

implementation
function StringsContain(s: TStrings; str: String): Boolean;
var
  i: Integer;
begin
  str:=LowerCase(str);
  Result:=False;
  for i:=0 to s.Count-1 do
    if LowerCase(s[i])=str then
    begin
      Result:=True;
      exit;
    end;
end;


function isEnd(s, endTok: string; ex: Boolean=false): boolean;

  function getFirstTok(str: string): string;
  var
    i, len: integer;
  begin
    len := 1;
    if length(str) > 1 then
    begin
      i := 2;
      if ex then
      while (str[i] in ['0'..'9', 'A'..'Z', 'a'..'z', '_', '-', '#','.',',']) do
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

function OpendFileInfo(Name: string; Line: integer = 1;
  Pos: integer = 1): TOpendFileInfo;
begin
  Result.Name := Name;
  Result.Line := Line;
  Result.Pos := Pos;
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
