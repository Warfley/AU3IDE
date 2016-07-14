unit ToolTip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Controls, Math, windows;

type
  TEditorToolTip = class(TGraphicControl)
  private
    FFunc: string;
    FParams: TStringList;
    FInfo: string;
    FSelectedParam: integer;
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
  p, i: integer;
  sl: TStringList;
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
  Canvas.TextOut(p, 1, FFunc + '(');
  Inc(p, Canvas.TextWidth(FFunc + '('));
  for i := 0 to FParams.Count - 1 do
  begin
    if SelectedParam = i then
      Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    Canvas.TextOut(p, 1, FParams[i]);
    Inc(p, Canvas.TextWidth(FParams[i]));
    Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    if i <> FParams.Count - 1 then
    begin
      Canvas.TextOut(p, 1, ',');
      Inc(p, Canvas.TextWidth(','));
    end;
  end;
  Canvas.TextOut(p, 1, ')');
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
