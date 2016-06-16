unit CodeFormatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, Dialogs;

type
  TCodeFormatter = class(TObject)
  private
    FLines: TStringList;
    procedure SetLines(l: TStringList);
    function isEnd(s, endTok: string): boolean;
    function FormatLine(ln: string): string;
    function GetSpaces(Depth: integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Format;
    property Lines: TStringList read FLines write SetLines;
  end;

implementation

function TCodeFormatter.isEnd(s, endTok: string): boolean;
var
  l, l2: integer;
begin
  s := Trim(s);
  l := Length(endTok);
  l2 := Length(s);
  Result := False;
  if l2 < l then
  begin
    Exit;
  end
  else
  if (l2 > l) and (AnsiStartsText(endTok, s) and (s[l + 1] in [#0..#32])) then
  begin
    Result := True;
    Exit;
  end
  else
  if LowerCase(s) = endTok then
  begin
    Result := True;
    Exit;
  end;
end;

procedure TCodeFormatter.SetLines(l: TStringList);
begin
  FLines.Clear;
  FLines.AddStrings(l);
end;

constructor TCodeFormatter.Create;
begin
  FLines := TStringList.Create;
end;

destructor TCodeFormatter.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TCodeFormatter.FormatLine(ln: string): string;
type
  TTokenType = (ttNormal, ttsymbol, ttNearSym, ttrightNear);

  function getTokenkind(token: string): TTokenType;
  begin
    if token[1] in ['_', '0'..'9', 'a'..'z', 'A'..'Z', '$', '"', ';', '#'] then
      Result := ttNormal
    else if token[1] in ['(', '[', '.'] then
      Result := ttNearSym
    else if token[1] in [')', ']', ','] then
      Result := ttrightNear
    else
      Result := ttsymbol;
  end;

var
  TokStart, TokEnd, TokLen, l: integer;
  i: integer;
  t1, t2: TTokenType;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    TokStart := 1;
    TokEnd := 1;
    l := Length(ln);
    while TokEnd <= l do
    begin
      TokStart := TokEnd;
      if TokStart > l then
        exit
      else
      if ln[TokEnd] in [#9, ' '] then
      begin
        while (TokEnd <= l) and (ln[TokEnd] in [#0..#32]) do
          Inc(TokEnd);
        TokLen := TokEnd - TokStart;
        sl.Add(copy(ln, TokStart, TokLen));
      end
      else if ln[TokEnd] = ';' then
      begin
        TokEnd := l + 1;
        TokLen := l - TokStart + 1;
        sl.Add(copy(ln, TokStart, TokLen));
      end
      else if not (ln[TokEnd] in ['_', '0'..'9', 'a'..'z', 'A'..'Z',
        '$', '"', '#']) then
      begin
        Inc(TokEnd);
        sl.Add(ln[TokStart]);
      end
  else if ln[TokEnd] = '#' then
  begin
    TokLen:=1;
    while ln[TokEnd+TokLen] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-'] do
      inc(TokLen);
    Inc(TokEnd, TokLen);
    sl.Add(copy(ln, TokStart, TokLen))
  end
      else if ln[TokEnd] = '$' then
      begin
        Inc(TokEnd);
        while ln[TokEnd] in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
          Inc(TokEnd);
        TokLen := TokEnd - TokStart;
        sl.Add(copy(ln, TokStart, TokLen));
      end
      else if ln[TokEnd] = '"' then
      begin
        Inc(TokEnd);
        while (TokEnd <= l) and (ln[TokEnd] <> '"') do
          Inc(TokEnd);
        Inc(TokEnd);
        TokLen := TokEnd - TokStart;
        sl.Add(copy(ln, TokStart, TokLen));
      end
      else if ln[TokEnd] in ['0'..'9'] then
      begin
        while (TokEnd <= l) and
          (ln[TokEnd] in ['0'..'9', '.', 'x', 'a'..'f', 'A'..'F']) do
          Inc(TokEnd);
        TokLen := TokEnd - TokStart;
        sl.Add(copy(ln, TokStart, TokLen));
      end
      else
      begin
        while ln[TokEnd] in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
          Inc(TokEnd);
        TokLen := TokEnd - TokStart;
        sl.Add(copy(ln, TokStart, TokLen));
      end;
    end;

    i := 0;
    while i < sl.Count do
      if Trim(sl[i]) = '' then
        sl.Delete(i)
      else
        Inc(i);
    i := 1;
    while i < sl.Count do
    begin
      t1 := getTokenkind(sl[i - 1]);
      t2 := getTokenkind(sl[i]);
      if not ((t1 = ttNearSym) or (t2 = ttNearSym) or (t2 = ttrightNear) or
        ((t1 = ttsymbol) and (t2 = ttsymbol))) then
      begin
        sl.Insert(i, ' ');
        Inc(i);
      end;
      Inc(i);
    end;
    Result := '';
    for i := 0 to sl.Count - 1 do
      Result := Result + sl[i];
  finally
    sl.Free;
  end;
end;

function TCodeFormatter.GetSpaces(Depth: integer): string;
var
  i: integer;
begin
  SetLength(Result, Depth * 2);
  FillChar(Result[1], Depth * 2, ' ');
end;

procedure TCodeFormatter.Format;
var
  i: integer;
  depth: integer;
begin
  depth := 0;
  for i := 0 to FLines.Count - 1 do
  begin
    if isEnd(FLines[i], 'endfunc') or isEnd(FLines[i], 'wend') or
      isEnd(FLines[i], 'next') or isEnd(FLines[i], 'endif') then
      Dec(depth)
    else
    if isEnd(FLines[i], 'func') or isEnd(FLines[i], 'while') or
      isEnd(FLines[i], 'for') or isEnd(FLines[i], 'if') or isEnd(FLines[i], 'else') then
    begin
      if not isEnd(FLines[i], 'else') then
        Inc(depth);
      FLines[i] := GetSpaces(depth - 1) + FormatLine(Trim(FLines[i]));
    end
    else
      FLines[i] := GetSpaces(depth) + FormatLine(Trim(FLines[i]));
  end;
end;

end.
