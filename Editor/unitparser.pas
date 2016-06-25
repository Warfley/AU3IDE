unit UnitParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, au3Types, strutils, Forms, Dialogs;

type
  TUnitParser = class(TThread)
  private
    FOnVarFound, FOnFuncFound, FOnFinished: TNotifyEvent;
    FText: TStringList;
    FFunc: TFuncList;
    FRanges: TObjectList;
    FVars: TVarList;
    FRequiredFiles: TStringList;
    FMyFunc: TFuncList;
    FMyRanges: TObjectList;
    FMyRequiredFiles: TStringList;
    FMYVars: TVarList;
    FCurr: TStringList;
    FWait: boolean;
    procedure UpdateTheShit(Data: IntPtr);
    procedure SetText(s: string);
    procedure ParseLine(ln: string; vars: TVarList; line: integer);
    procedure ParseRange(var i: integer; endTok: string; RType: TRangeType);
  protected
    procedure Execute; override;
  public
    property RequiredFiles: TStringList read FRequiredFiles write FRequiredFiles;
    property Text: string write SetText;
    property Funcs: TFuncList read FFunc write FFunc;
    property Ranges: TObjectList read FRanges write FRanges;
    property Vars: TVarList read FVars write FVars;
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    property OnVarFound: TNotifyEvent read FOnVarFound write FOnFuncFound;
    property OnFuncFound: TNotifyEvent read FOnFuncFound write FOnFuncFound;
    property OnFinished: TNotifyEvent read FOnFinished write FOnFinished;
  end;

implementation

function StringsContain(s: TStrings; str: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to s.Count - 1 do
    if LowerCase(str) = LowerCase(s[i]) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TUnitParser.SetText(s: string);
begin
  FText.Text := s;
end;

constructor TUnitParser.Create(CreateSuspended: boolean);
begin
  FMyFunc := TFuncList.Create;
  FMyRanges := TObjectList.Create(False);
  FMYVars := TVarList.Create;
  FCurr := TStringList.Create;
  FText := TStringList.Create;
  FMyRequiredFiles := TStringList.Create;
  FreeOnTerminate := False;
  inherited Create(CreateSuspended);
end;

destructor TUnitParser.Destroy;
begin
  FMYVars.Free;
  FMyFunc.Free;
  FMyRanges.Free;
  FMyRequiredFiles.Free;
  FCurr.Free;
  FText.Free;
  inherited Destroy;
end;

procedure TUnitParser.ParseRange(var i: integer; endTok: string; RType: TRangeType);

  function searchfor(s: TStrings; str: string; out n: integer): boolean;
  var
    i: integer;
  begin
    n := -1;
    Result := False;
    for i := 0 to s.Count - 1 do
      if LowerCase(s[i]) = LowerCase(str) then
      begin
        n := i;
        Result := True;
        Exit;
      end;
  end;

var
  x, n: integer;
  ln: string;
  curr: TDefRange;
begin
  if i >= FText.Count then
    Exit;
  curr := TDefRange.Create;
  curr.RangeType := RType;
  ln := FText[i];
  curr.StartLine := i;
  ParseLine(ln, curr.Vars, i);
  Inc(i);
  while (i < FText.Count) and (not isEnd(LowerCase(ln), endTok)) and not Terminated do
  begin
    ln := FText[i];
    if isEnd(ln, 'if') then
      ParseRange(i, 'endif', rtIf)
    else if isEnd(ln, 'while') then
      ParseRange(i, 'wend', rtWhile)
    else if isEnd(ln, 'for') then
      ParseRange(i, 'next', rtFor)
    else
      ParseLine(ln, curr.Vars, i);
    Inc(i);
  end;
  Dec(i);
  curr.EndLine := i;
  FMyRanges.Add(curr);
  for x := 0 to curr.Vars.Count - 1 do
    if searchfor(FCurr, curr.Vars[x].Name, n) then
      FCurr.Delete(n);
end;

procedure TUnitParser.ParseLine(ln: string; vars: TVarList; line: integer);
function InOtherFile(v: String): Boolean;
var
  i: Integer;
begin
  Result:=False;
  for i:=0 to FVars.Count-1 do
    if (lowercase(FVars[i].Name)=LowerCase(v)) and (FVars[i].FileName<>'') then
    begin
      Result:=True;
      Break;
    end;
end;

var
  i, s, len: integer;
  str: string;
begin
  i := 1;
  while (i <= Length(ln)) and not Terminated do
  begin
    if ln[i] = '$' then
    begin
      s := i;
      len := 1;
      Inc(i);
      while (i <= Length(ln)) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
      begin
        Inc(i);
        Inc(len);
      end;
      str := Copy(ln, s, len);
      if (i <= Length(ln)) and (ln[i] = '[') then
        str := str + '[]';
      if len > 1 then
        if not StringsContain(FCurr, str) and not InOtherFile(str) then
        begin
          if isEnd(ln, 'global') then
            FMYVars.Add(VarInfo(str, line, s))
          else
            vars.Add(VarInfo(str, line, s));
          FCurr.Add(str);
          if Assigned(FOnVarFound) then
            Application.QueueAsyncCall(TDataEvent(FOnVarFound), PtrInt(Self));
        end;
    end;
    Inc(i);
  end;
end;

procedure TUnitParser.Execute;
var
  i, x, s, len: integer;
  str, ln: string;
  sl: TStringList;
begin
  FCurr.Clear;
  FMyFunc.Clear;
  FMYVars.Clear;
  FMyRanges.Clear;
  i := 0;
  sl := TStringList.Create;
  try
    while (i < FText.Count) and not Terminated do
    begin
      ln := trim(FText[i]);

      if AnsiStartsStr(';*', ln) then
        sl.Add(Copy(ln, 3, Length(ln)))
      else
      if isEnd(ln, '#include') then
      begin
        if pos('<', ln) > 0 then
        begin
          str := ln;
          Delete(str, 1, pos('<', str));
          if pos('>', str) = 0 then
          begin
            Inc(i);
            Continue;
          end;
          Delete(str, Pos('>', str), length(str));
          str:=Trim(str);
          if not StringsContain(FMyRequiredFiles, str) then
            FMyRequiredFiles.Add(str);
        end;
      end
      else
      if isEnd(ln, 'func') then
      begin
        len := 0;
        s := 5;
        for x := 5 to Length(ln) do
          if ln[x] in [#0..#32] then
            Inc(s)
          else
            Break;
        for x := s to Length(ln) do
        begin
          Inc(len);
          if ln[x] = ')' then
            Break;
        end;
        str := Copy(ln, s, len);
        FMyFunc.Add(FuncInfo(str, i, sl.Text));
        sl.Clear;
        if Assigned(FOnFuncFound) then
          Application.QueueAsyncCall(TDataEvent(FOnFuncFound), PtrInt(self));
        ParseRange(i, 'endfunc', rtFunc);
      end
      else if isEnd(ln, 'if') then
      begin
        sl.Clear;
        ParseRange(i, 'endif', rtIf);
      end
      else if isEnd(ln, 'while') then
      begin
        sl.Clear;
        ParseRange(i, 'wend', rtWhile);
      end
      else if isEnd(ln, 'for') then
      begin
        sl.Clear;
        ParseRange(i, 'next', rtFor);
      end
      else
      begin
        if ln <> '' then
          sl.Clear;
        ParseLine(ln, FMyVars, i);
      end;
      Inc(i);
    end;
  finally
    sl.Free;
  end;
  if Terminated then
    Exit;
  FWait := True;
  Application.QueueAsyncCall(@UpdateTheShit, 0);
  while FWait do
    Sleep(20);
end;

procedure TUnitParser.UpdateTheShit(Data: IntPtr);
var
  i: integer;
begin
  FFunc.Assign(FMyFunc);
  FVars.Assign(FMYVars);
  if Assigned(FRanges) then
  begin
    for i := 0 to FRanges.Count - 1 do
      FRanges[i].Free;
    FRanges.Clear;
    for i := 0 to FMyRanges.Count - 1 do
      FRanges.Add(FMyRanges[i]);
  end
  else
    for i:=0 to FMyRanges.Count-1 do
      FMyRanges[i].Free;
  FRequiredFiles.Clear;
  FRequiredFiles.AddStrings(FMyRequiredFiles);
  FWait := False;
  if Assigned(FOnFinished) then
    Application.QueueAsyncCall(TDataEvent(FOnFinished), PtrInt(self));
end;

end.
