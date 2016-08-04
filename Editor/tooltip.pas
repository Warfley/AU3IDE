unit ToolTip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Controls, Math, au3Highlighter, au3Types;

type
  TEditorToolTip = class(TGraphicControl)
  private
    FFunc: string;
    FParams: TStringList;
    FInfo: string;
    FSelectedParam: integer;
    hl: Tau3SynHighlight;
    procedure SetSelectedParam(x: integer);
    function GetFunc: string;
    function GetSummarySize: integer;
    function GetSummaryWidth: integer;
    procedure SetFunc(func: string);
    procedure SetInfo(inf: string);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowAt(X, Y, LineHight: integer);
    property SelectedParam: integer read FSelectedParam write SetSelectedParam;
    property Func: string read getFunc write SetFunc;
    property Info: string read FInfo write SetInfo;
    property Highlighter: Tau3SynHighlight read hl write hl;
  end;

implementation

function TEditorToolTip.GetSummarySize: integer;
var
  sl: TStringList;
var
  i: integer;
begin
  Result := 0;
  sl := TStringList.Create;
  try
    sl.Text := FInfo;
    for i := 0 to sl.Count - 1 do
      Result := Result + Canvas.TextHeight(sl[i]);
  finally
    sl.Free;
  end;
end;

function TEditorToolTip.GetSummaryWidth: integer;
var
  sl: TStringList;
var
  i: integer;
begin
  Result := 0;
  sl := TStringList.Create;
  try
    sl.Text := FInfo;
    for i := 0 to sl.Count - 1 do
      Result := Max(Result, Canvas.GetTextWidth(sl[i]));
  finally
    sl.Free;
  end;
end;

procedure TEditorToolTip.SetSelectedParam(x: integer);
begin
  FSelectedParam := x;
  Invalidate;
end;

function TEditorToolTip.GetFunc: string;
begin
  FParams.Delimiter := ',';
  FParams.StrictDelimiter := True;
  Result := FFunc + '(' + FParams.DelimitedText + ')';
end;

procedure TEditorToolTip.SetFunc(func: string);
var
  i: integer;
begin
  FFunc := Copy(func, 1, Pos('(', func) - 1);
  FParams.Clear;
  FParams.Delimiter := ',';
  FParams.StrictDelimiter := True;
  FParams.DelimitedText := Copy(func, Pos('(', func) + 1, Length(func) -
    Pos('(', func) - 1);
  for i := 0 to FParams.Count - 1 do
    FParams[i] := Trim(FParams[i]);
  Self.Width := Max(Canvas.TextWidth(GetFunc), GetSummaryWidth) + 4;
  self.Height := Canvas.TextHeight(GetFunc) + GetSummarySize + 4;
  Invalidate;
end;

procedure TEditorToolTip.SetInfo(inf: string);
begin
  FInfo := inf;
  Self.Width := Max(Canvas.TextWidth(GetFunc), Canvas.TextWidth(FInfo)) + 4;
  self.Height := Canvas.TextHeight(GetFunc) + Canvas.TextHeight(FInfo) + 4;
  Invalidate;
end;

procedure TEditorToolTip.Paint;
var
  p, i, s,l: integer;
  sl: TStringList;
  AKey, tok: String;
  curr: TTokenType;
begin
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := $00333333;
  Canvas.Rectangle(0, 0, Width, Height);
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psClear;
  Canvas.Font := Font;
  p := 2;
  Canvas.Font.Color:=hl.FunctionAttribute.Foreground;
  Canvas.TextOut(p, 1, FFunc);
  Inc(p, Canvas.TextWidth(FFunc));
  Canvas.Font.Color:=hl.SymbolAttribute.Foreground;
  Canvas.TextOut(p, 1, '(');
  Inc(p, Canvas.TextWidth('('));
  for i := 0 to FParams.Count - 1 do
  begin
    AKey:=FParams[i];
    With Canvas do
    if Length(AKey)>0 then
    begin
      s := 1;
      while s <= Length(AKey) do
      begin
        case AKey[s] of
          '$': curr := tkVar;
          '#': curr := tkFunction;
          '@', '0'..'9': curr := tkNumber;
          'A'..'Z', 'a'..'z', '_': curr := tkUnknown;
          '"': curr := tkString;
          '{': curr := tkTemp;
          #01..#32: curr := tkSpace;
          else
            curr := tkSymbol;
        end;
        l := 1;
        while s + l <= Length(AKey) do
          if ((AKey[s + l] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-']) and
            (curr in [tkVar, tkFunction, tkUnknown, tkNumber])) or
            ((AKey[s + l] in [#01..#32]) and (curr = tkSpace)) or
            ((AKey[s + l] <> '"') and (curr = tkString)) or
            ((AKey[s + l] <> '}') and (curr = tkTemp)) then
            Inc(l)
          else
            break;
        if curr in [tkString, tkTemp] then
          Inc(l) ;
        tok := Copy(AKey, s, l);
        case curr of
          tkVar:
            Font.Color := hl.VariableAttribute.Foreground;
          tkFunction:
            Font.Color := hl.FunctionAttribute.Foreground;
          tkUnknown:
            Font.Color := hl.NumberAttribute.Foreground;
          tkNumber:
            Font.Color := hl.NumberAttribute.Foreground;
          tkString:
            Font.Color := hl.StringAttribute.Foreground;
          tkSpace:
            Font.Color := hl.SpaceAttribute.Foreground;
          tkSymbol:
            Font.Color := hl.SymbolAttribute.Foreground;
          tkTemp:
            Font.Color := hl.TempAttribute.Foreground;
        end;
    Canvas.Font.Bold:=SelectedParam=i;
        TextOut(p, 1, tok);
        Inc(s, l);
        Inc(p, TextWidth(tok));
      end;
    end;
    Canvas.Font.Bold:=False;
    if i <> FParams.Count - 1 then
    begin
  Canvas.Font.Color:=hl.SymbolAttribute.Foreground;
      Canvas.TextOut(p, 1, ',');
      Inc(p, Canvas.TextWidth(', '));
    end;
  end;
  Canvas.Font.Color:=hl.SymbolAttribute.Foreground;
  Canvas.TextOut(p, 1, ')');
  Canvas.Font.Color:=hl.CommentAttribute.Foreground;
  p:=Canvas.TextHeight(FFunc)+3;
  sl:=TStringList.Create;
  try
    sl.Text:=FInfo;
    for i:=0 to sl.Count-1 do
    begin
      Canvas.TextOut(1, p, sl[i]);
      inc(p, Canvas.TextHeight(sl[i]));
    end;
  finally
    sl.Free;
  end;
  inherited;
end;

constructor TEditorToolTip.Create(AOwner: TComponent);
begin
  inherited;
  FParams := TStringList.Create;
  FSelectedParam := -1;
  Visible := False;
end;

destructor TEditorToolTip.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TEditorToolTip.ShowAt(X, Y, LineHight: integer);
begin
  if x + Width > Parent.ClientWidth then
    x := Max(0, Parent.ClientWidth - Self.Width);
  if y - Height < 0 then
    y := y + LineHight
  else
    y := y - Height;
  Left := X;
  Top := Y;
  Visible := True;
end;

end.
