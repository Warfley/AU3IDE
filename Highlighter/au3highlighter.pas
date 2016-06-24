unit au3Highlighter;

{$mode objfpc}{$H+}
{$Define CaseInsensitive}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter,
  SynEditHighlighterFoldBase, Dialogs, au3Types;

type

  { Tau3SynHighlight }

  Tau3SynHighlight = class(TSynCustomFoldHighlighter)
  private
    FStrAttr, FTempAttr, FDocAttr, FCommentAttr, FIdentifierAttr, FKeyAttr,
    FFunctionAttr, FNumberAttr, FSpaceAttr, FTextAttr, FVarAttr,
    FSelectAttr: TSynHighlighterAttributes;
    FSelectText: string;
    CurrLine: integer;
    FRange: IntPtr;
    FCurRange: IntPtr;
    procedure SetDocAttr(v: TSynHighlighterAttributes);
    procedure SetStrAttr(v: TSynHighlighterAttributes);
    procedure SetComAttr(v: TSynHighlighterAttributes);
    procedure SetIdentAttr(v: TSynHighlighterAttributes);
    procedure SetKeyAttr(v: TSynHighlighterAttributes);
    procedure SetFuncAttr(v: TSynHighlighterAttributes);
    procedure SetNumAttr(v: TSynHighlighterAttributes);
    procedure SetSpaceAttr(v: TSynHighlighterAttributes);
    procedure SetTextAttr(v: TSynHighlighterAttributes);
    procedure SetVarAttr(v: TSynHighlighterAttributes);
    procedure SetSelectAttr(v: TSynHighlighterAttributes);
    procedure SetTempAttr(v: TSynHighlighterAttributes);
    function GetLine: string;
  protected
    FTokenPos, FTokenEnd, FLineNum: integer;
    FToken: string;
    FTok: TTokenType;
    FTokLen: integer;
    FTokenHash: byte;
    FLineText: string;
    FHashList: array[0..255] of TList;
    function GetAttr(a: TTokenType): TSynHighlighterAttributes;
  public
    procedure LoadConfig(Path: string);
    function HashToken(toHash: PChar; out Len: integer): byte;
    procedure CheckHash;
    procedure SetLine(const NewValue: string; LineNumber: integer); override;
    procedure Next; override;
    function GetEol: boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  public
    property Line: string read GetLine;
    function GetToken: string; override;
    function GetTokenPos: integer; override;
    function GetTokenKind: integer; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedText: string read FSelectText write FSelectText;
  published
    property DocumentaryAttribute: TSynHighlighterAttributes
      read FDocAttr write SetDocAttr;
    property StringAttribute: TSynHighlighterAttributes read FStrAttr write SetStrAttr;
    property IdentifierAttribute: TSynHighlighterAttributes
      read FIdentifierAttr write SetIdentAttr;
    property CommentAttribute: TSynHighlighterAttributes
      read FCommentAttr write SetComAttr;
    property KeywordAttribute: TSynHighlighterAttributes read FKeyAttr write SetKeyAttr;
    property FunctionAttribute: TSynHighlighterAttributes
      read FFunctionAttr write SetFuncAttr;
    property NumberAttribute: TSynHighlighterAttributes
      read FNumberAttr write SetNumAttr;
    property SpaceAttribute: TSynHighlighterAttributes
      read FSpaceAttr write SetSpaceAttr;
    property TextAttribute: TSynHighlighterAttributes read FTextAttr write SetTextAttr;
    property VariableAttribute: TSynHighlighterAttributes read FVarAttr write SetVarAttr;
    property SelectAttribute: TSynHighlighterAttributes
      read FSelectAttr write SetSelectAttr;
    property TempAttribute: TSynHighlighterAttributes read FTempAttr write SetTempAttr;
  end;

const
  IfBlockID = 1;
  WhileBlockID = 2;
  FuncBlockID = 3;
  CommentBlockID = 4;
  ForBlockID = 5;
  UntilBlockID = 6;
  SelectBlockID = 7;
  SwitchBlockID = 8;
WithBlockID = 9;

implementation

function Tau3SynHighlight.GetLine: string;
begin
  Result := FLineText;
end;

procedure Tau3SynHighlight.SetDocAttr(v: TSynHighlighterAttributes);
begin
  FDocAttr.Assign(v);
end;

procedure Tau3SynHighlight.SetStrAttr(v: TSynHighlighterAttributes);
begin
  FStrAttr.Assign(v);
end;

procedure Tau3SynHighlight.SetComAttr(v: TSynHighlighterAttributes);
begin
  FCommentAttr.Assign(v);
end;

procedure Tau3SynHighlight.SetIdentAttr(v: TSynHighlighterAttributes);
begin
  FIdentifierAttr.Assign(v);
end;

procedure Tau3SynHighlight.SetKeyAttr(v: TSynHighlighterAttributes);
begin
  FKeyAttr.Assign(v);
end;

procedure Tau3SynHighlight.SetFuncAttr(v: TSynHighlighterAttributes);
begin
  FFunctionAttr.Assign(v);
end;

procedure Tau3SynHighlight.SetNumAttr(v: TSynHighlighterAttributes);
begin
  FNumberAttr.Assign(v);
end;

procedure Tau3SynHighlight.SetSpaceAttr(v: TSynHighlighterAttributes);
begin
  FSpaceAttr.Assign(v);
end;

procedure Tau3SynHighlight.SetTextAttr(v: TSynHighlighterAttributes);
begin
  FTextAttr.Assign(v);
end;

procedure Tau3SynHighlight.SetVarAttr(v: TSynHighlighterAttributes);
begin
  FVarAttr.Assign(v);
end;

procedure Tau3SynHighlight.SetSelectAttr(v: TSynHighlighterAttributes);
begin
  FSelectAttr.Assign(v);
end;

procedure Tau3SynHighlight.SetTempAttr(v: TSynHighlighterAttributes);
begin
  FTempAttr.Assign(v);
end;

procedure Tau3SynHighlight.CheckHash;

  function KeyComp(aKey: string): boolean;
  var
    i: integer;
    t: string;
  begin
    {$IfDef CaseInsensitive}
    t := LowerCase(FToken);
    aKey := LowerCase(aKey);
    {$Else}
    t := FToken;
    {$EndIf}
    if Length(aKey) <> FTokLen then
    begin
      Result := False;
      Exit;
    end;
    Result := True;
    for i := 1 to FTokLen do
      if t[i] <> aKey[i] then
      begin
        Result := False;
        Break;
      end;
  end;

var
  i: integer;
begin
  FTok := tkUnknown;
  for i := 0 to FHashList[FTokenHash].Count - 1 do
    if KeyComp(PHashInfo(FHashList[FTokenHash][i])^.Key) then
    begin
      FTok := PHashInfo(FHashList[FTokenHash][i])^.Kind;
      Break;
    end;
end;


procedure Tau3SynHighlight.LoadConfig(Path: string);

  procedure LoadHLTable(p: string);
  var
    fs: TFileStream;
    tmp: PHashInfo;
    i, x, n, a: integer;
  begin
    fs := TFileStream.Create(p, fmOpenRead);
    try
      for x := 0 to 255 do
      begin
        fs.Read(n, SizeOf(n));
        for i := 0 to n - 1 do
        begin
          new(tmp);
          fs.Read(tmp^.Kind, SizeOf(tmp^.Kind));
          fs.Read(a, SizeOf(a));
          SetLength(tmp^.Key, a);
          fs.Read(tmp^.Key[1], a);
          FHashList[x].Add(tmp);
        end;
      end;
    finally
      fs.Free;
    end;
  end;

  procedure LoadHLConfig(p: string);
  type
    TFontInfo = packed record
      FontCol: TColor;
      Big, Italics, Underline, Frame: boolean;
      FrameColor: TColor;
      Background: boolean;
      BackColor: TColor;
    end;

  var
    tmp: TFontInfo;
    fs: TFileStream;
  begin
    fs := TFileStream.Create(p, fmOpenRead);
    try
      // FCommentAttr;
      fs.Read(tmp, SizeOf(tmp));
      FCommentAttr.Foreground := tmp.FontCol;
      FCommentAttr.Style := [];
      if tmp.Big then
        FCommentAttr.Style := FCommentAttr.Style + [fsBold];
      if tmp.Italics then
        FCommentAttr.Style := FCommentAttr.Style + [fsItalic];
      if tmp.Underline then
        FCommentAttr.Style := FCommentAttr.Style + [fsUnderline];
      if tmp.Frame then
        FCommentAttr.FrameEdges := sfeAround
      else
        FCommentAttr.FrameEdges := sfeNone;
      FCommentAttr.FrameColor := tmp.FrameColor;
      if tmp.Background then
        FCommentAttr.Background := tmp.BackColor
      else
        FCommentAttr.Background := clNone;
      // FIdentifierAttr;
      fs.Read(tmp, SizeOf(tmp));
      FIdentifierAttr.Foreground := tmp.FontCol;
      FIdentifierAttr.Style := [];
      if tmp.Big then
        FIdentifierAttr.Style := FIdentifierAttr.Style + [fsBold];
      if tmp.Italics then
        FIdentifierAttr.Style := FIdentifierAttr.Style + [fsItalic];
      if tmp.Underline then
        FIdentifierAttr.Style := FIdentifierAttr.Style + [fsUnderline];
      if tmp.Frame then
        FIdentifierAttr.FrameEdges := sfeAround
      else
        FIdentifierAttr.FrameEdges := sfeNone;
      FIdentifierAttr.FrameColor := tmp.FrameColor;
      if tmp.Background then
        FIdentifierAttr.Background := tmp.BackColor
      else
        FIdentifierAttr.Background := clNone;
      // FFunctionAttr
      fs.Read(tmp, SizeOf(tmp));
      FFunctionAttr.Foreground := tmp.FontCol;
      FFunctionAttr.Style := [];
      if tmp.Big then
        FFunctionAttr.Style := FFunctionAttr.Style + [fsBold];
      if tmp.Italics then
        FFunctionAttr.Style := FFunctionAttr.Style + [fsItalic];
      if tmp.Underline then
        FFunctionAttr.Style := FFunctionAttr.Style + [fsUnderline];
      if tmp.Frame then
        FFunctionAttr.FrameEdges := sfeAround
      else
        FFunctionAttr.FrameEdges := sfeNone;
      FFunctionAttr.FrameColor := tmp.FrameColor;
      if tmp.Background then
        FFunctionAttr.Background := tmp.BackColor
      else
        FFunctionAttr.Background := clNone;
      // FKeyAttr;
      fs.Read(tmp, SizeOf(tmp));
      FKeyAttr.Foreground := tmp.FontCol;
      FKeyAttr.Style := [];
      if tmp.Big then
        FKeyAttr.Style := FKeyAttr.Style + [fsBold];
      if tmp.Italics then
        FKeyAttr.Style := FKeyAttr.Style + [fsItalic];
      if tmp.Underline then
        FKeyAttr.Style := FKeyAttr.Style + [fsUnderline];
      if tmp.Frame then
        FKeyAttr.FrameEdges := sfeAround
      else
        FKeyAttr.FrameEdges := sfeNone;
      FKeyAttr.FrameColor := tmp.FrameColor;
      if tmp.Background then
        FKeyAttr.Background := tmp.BackColor
      else
        FKeyAttr.Background := clNone;
      // FNumberAttr;
      fs.Read(tmp, SizeOf(tmp));
      FNumberAttr.Foreground := tmp.FontCol;
      FNumberAttr.Style := [];
      if tmp.Big then
        FNumberAttr.Style := FNumberAttr.Style + [fsBold];
      if tmp.Italics then
        FNumberAttr.Style := FNumberAttr.Style + [fsItalic];
      if tmp.Underline then
        FNumberAttr.Style := FNumberAttr.Style + [fsUnderline];
      if tmp.Frame then
        FNumberAttr.FrameEdges := sfeAround
      else
        FNumberAttr.FrameEdges := sfeNone;
      FNumberAttr.FrameColor := tmp.FrameColor;
      if tmp.Background then
        FNumberAttr.Background := tmp.BackColor
      else
        FNumberAttr.Background := clNone;
      // FSpaceAttr;
      fs.Read(tmp, SizeOf(tmp));
      FSpaceAttr.Foreground := tmp.FontCol;
      FSpaceAttr.Style := [];
      if tmp.Big then
        FSpaceAttr.Style := FSpaceAttr.Style + [fsBold];
      if tmp.Italics then
        FSpaceAttr.Style := FSpaceAttr.Style + [fsItalic];
      if tmp.Underline then
        FSpaceAttr.Style := FSpaceAttr.Style + [fsUnderline];
      if tmp.Frame then
        FSpaceAttr.FrameEdges := sfeAround
      else
        FSpaceAttr.FrameEdges := sfeNone;
      FSpaceAttr.FrameColor := tmp.FrameColor;
      if tmp.Background then
        FSpaceAttr.Background := tmp.BackColor
      else
        FSpaceAttr.Background := clNone;
      // FStrAttr;
      fs.Read(tmp, SizeOf(tmp));
      FStrAttr.Foreground := tmp.FontCol;
      FStrAttr.Style := [];
      if tmp.Big then
        FStrAttr.Style := FStrAttr.Style + [fsBold];
      if tmp.Italics then
        FStrAttr.Style := FStrAttr.Style + [fsItalic];
      if tmp.Underline then
        FStrAttr.Style := FStrAttr.Style + [fsUnderline];
      if tmp.Frame then
        FStrAttr.FrameEdges := sfeAround
      else
        FStrAttr.FrameEdges := sfeNone;
      FStrAttr.FrameColor := tmp.FrameColor;
      if tmp.Background then
        FStrAttr.Background := tmp.BackColor
      else
        FStrAttr.Background := clNone;
      // FVarAttr;
      fs.Read(tmp, SizeOf(tmp));
      FVarAttr.Foreground := tmp.FontCol;
      FVarAttr.Style := [];
      if tmp.Big then
        FVarAttr.Style := FVarAttr.Style + [fsBold];
      if tmp.Italics then
        FVarAttr.Style := FVarAttr.Style + [fsItalic];
      if tmp.Underline then
        FVarAttr.Style := FVarAttr.Style + [fsUnderline];
      if tmp.Frame then
        FVarAttr.FrameEdges := sfeAround
      else
        FVarAttr.FrameEdges := sfeNone;
      FVarAttr.FrameColor := tmp.FrameColor;
      if tmp.Background then
        FVarAttr.Background := tmp.BackColor
      else
        FVarAttr.Background := clNone;
      // FDocAttr;
      fs.Read(tmp, SizeOf(tmp));
      FDocAttr.Foreground := tmp.FontCol;
      FDocAttr.Style := [];
      if tmp.Big then
        FDocAttr.Style := FDocAttr.Style + [fsBold];
      if tmp.Italics then
        FDocAttr.Style := FDocAttr.Style + [fsItalic];
      if tmp.Underline then
        FDocAttr.Style := FDocAttr.Style + [fsUnderline];
      if tmp.Frame then
        FDocAttr.FrameEdges := sfeAround
      else
        FDocAttr.FrameEdges := sfeNone;
      FDocAttr.FrameColor := tmp.FrameColor;
      if tmp.Background then
        FDocAttr.Background := tmp.BackColor
      else
        FVarAttr.Background := clNone;
      // FTextAttr;
      fs.Read(tmp, SizeOf(tmp));
      FTextAttr.Foreground := tmp.FontCol;
      FTextAttr.Style := [];
      if tmp.Big then
        FTextAttr.Style := FTextAttr.Style + [fsBold];
      if tmp.Italics then
        FTextAttr.Style := FTextAttr.Style + [fsItalic];
      if tmp.Underline then
        FTextAttr.Style := FTextAttr.Style + [fsUnderline];
      if tmp.Frame then
        FTextAttr.FrameEdges := sfeAround
      else
        FTextAttr.FrameEdges := sfeNone;
      FTextAttr.FrameColor := tmp.FrameColor;
      if tmp.Background then
        FTextAttr.Background := tmp.BackColor
      else
        FTextAttr.Background := clNone;
    finally
      fs.Free;
    end;
  end;

begin
  LoadHLTable(IncludeTrailingPathDelimiter(Path) + 'Keywords.lst');
  LoadHLConfig(IncludeTrailingPathDelimiter(Path) + 'Colors.cnf');
end;

function Tau3SynHighlight.GetAttr(a: TTokenType): TSynHighlighterAttributes;
begin
  Result := nil;
  case a of
    tkComment: Result := FCommentAttr;
    tkIdentifier: Result := FIdentifierAttr;
    tkFunction: Result := FFunctionAttr;
    tkSymbol: Result := FKeyAttr;
    tkNumber: Result := FNumberAttr;
    tkSpace: Result := FSpaceAttr;
    tkString: Result := FStrAttr;
    tkDoc: Result := FDocAttr;
    tkVar: Result := FVarAttr;
    tkTemp: Result := FTempAttr;
    else
      Result := FTextAttr;
  end;
end;

constructor Tau3SynHighlight.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited Create(AOwner);
  FStrAttr := TSynHighlighterAttributes.Create('String', 'String');
  FCommentAttr := TSynHighlighterAttributes.Create('Comment', 'Comment');
  FIdentifierAttr := TSynHighlighterAttributes.Create('Indentifier', 'Identifier');
  FKeyAttr := TSynHighlighterAttributes.Create('Symbol', 'Symbol');
  FFunctionAttr := TSynHighlighterAttributes.Create('Function', 'Function');
  FNumberAttr := TSynHighlighterAttributes.Create('Number', 'Number');
  FSpaceAttr := TSynHighlighterAttributes.Create('Space', 'Space');
  FTextAttr := TSynHighlighterAttributes.Create('Text', 'Text');
  FVarAttr := TSynHighlighterAttributes.Create('Variable', 'Variable');
  FSelectAttr := TSynHighlighterAttributes.Create('Selected', 'Selected');
  FDocAttr := TSynHighlighterAttributes.Create('Doc', 'Doc');
  FTempAttr:=TSynHighlighterAttributes.Create('Temp', 'Temp');
  FTempAttr.Foreground:=clGray;
  FTempAttr.Background:=clSilver;
  FTempAttr.FrameEdges:=sfeAround;
  FTempAttr.FrameColor:=clGray;
  FSelectText := '';
  for i := 0 to 255 do
    FHashList[i] := TList.Create;
end;

procedure Tau3SynHighlight.SetLine(const NewValue: string; LineNumber: integer);
begin
  inherited;
  FLineText := NewValue;
  // Next will start at "FTokenEnd", so set this to 1
  FTokenEnd := 1;
  CurrLine := LineNumber;
  Next;
end;

function Tau3SynHighlight.HashToken(toHash: PChar; out Len: integer): byte;
begin
  Result := 0;
  Len := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    {$IfDef CaseInsensitive}
    if (toHash^ in ['A'..'Z']) then
      Result := (integer(Result) + Ord(toHash^) - Ord('A') + Ord('a')) mod 256
    else
    {$EndIf}
      Result := (integer(Result) + Ord(toHash^)) mod 256;
    Inc(ToHash);
    Inc(Len);
  end;
end;

procedure Tau3SynHighlight.Next;
var
  l: integer;
begin
  // FTokenEnd should be at the start of the next Token (which is the Token we want)
  FTokenPos := FTokenEnd;
  // assume empty, will only happen for EOL
  FTokenEnd := FTokenPos;

  // Scan forward
  // FTokenEnd will be set 1 after the last char. That is:
  // - The first char of the next token
  // - or past the end of line (which allows GetEOL to work)

  l := length(FLineText);
  if FTokenPos > l then
    exit
  else
  if FLineText[FTokenEnd] in [#9, ' '] then
  begin
    while (FTokenEnd <= l) and (FLineText[FTokenEnd] in [#0..#32]) do
      Inc(FTokenEnd);
    FTokLen := FTokenEnd - FTokenPos;
    FToken := copy(FLineText, FTokenPos, FTokLen);
    FTok := tkSpace;
  end
  else if FLineText[FTokenEnd] = ';' then
  begin
    if (FTokenEnd < l) and (FLineText[FTokenEnd + 1] = '*') then
      FTok := tkDoc
    else
      FTok := tkComment;
    FTokenEnd := l + 1;
    FTokLen := l - FTokenPos + 1;
    FToken := Copy(FLineText, FTokenPos, FTokLen);
  end
  else if (FLineText[FTokenEnd] = '{') then
  begin
    Inc(FTokenEnd);
    FTokLen:=FTokenEnd;
    while (FTokLen <= l) and (FLineText[FTokLen] <> '}') do
      Inc(FTokLen);
    if (FTokLen > l) then
    begin
      FTokLen:=1;
      FToken:='{';
      FTok:=tkSymbol;
    end
    else
    begin   
      FTok := tkTemp;
      FTokenEnd:=FTokLen;
      Inc(FTokenEnd);
      FTokLen := FTokenEnd - FTokenPos;
      FToken := copy(FLineText, FTokenPos, FTokLen);
    end;
  end
  else if not (FLineText[FTokenEnd] in ['_', '0'..'9', 'a'..'z',
    'A'..'Z', '$', '"', '#', '@']) then
  begin
    Inc(FTokenEnd);
    FToken := FLineText[FTokenPos];
    FTokLen := 1;
    FTok := tkSymbol;
  end  
  else if FLineText[FTokenEnd] = '@' then
  begin
    Inc(FTokenEnd);
    FTokenHash := HashToken(@FLineText[FTokenEnd], FTokLen);
    Inc(FTokenEnd, FTokLen);
    Inc(FTokLen);
    FToken := copy(FLineText, FTokenPos, FTokLen);
    FTok := tkNumber;
  end
  else if FLineText[FTokenEnd] = '$' then
  begin
    Inc(FTokenEnd);
    FTokenHash := HashToken(@FLineText[FTokenEnd], FTokLen);
    Inc(FTokenEnd, FTokLen);
    Inc(FTokLen);
    FToken := copy(FLineText, FTokenPos, FTokLen);
    FTok := tkVar;
  end
  else if FLineText[FTokenEnd] = '#' then
  begin
    FTokLen := 1;
    while FLineText[FTokenEnd + FTokLen] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-'] do
      Inc(FTokLen);
    Inc(FTokenEnd, FTokLen);
    FToken := copy(FLineText, FTokenPos, FTokLen);
    FTok := tkFunction;
    if isEnd(FToken, '#cs') or isEnd(FToken, '#comments-start', true) then
    begin
      Inc(FCurRange);
      StartCodeFoldBlock(Pointer(CommentBlockID));
    end
    else if isEnd(FToken, '#ce') or isEnd(FToken, '#comments-end', True) then
    begin
      FTok:=tkComment;
      Dec(FCurRange);
      EndCodeFoldBlock();
    end;
  end
  else if FLineText[FTokenEnd] = '"' then
  begin
    Inc(FTokenEnd);
    FTok := tkString;
    while (FTokenEnd <= l) and (FLineText[FTokenEnd] <> '"') do
      Inc(FTokenEnd);
    Inc(FTokenEnd);
    FTokLen := FTokenEnd - FTokenPos;
    FToken := copy(FLineText, FTokenPos, FTokLen);
  end
  else if FLineText[FTokenEnd] in ['0'..'9'] then
  begin
    FTok := tkNumber;
    while (FTokenEnd <= l) and (FLineText[FTokenEnd] in
        ['0'..'9', '.', 'x', 'a'..'f', 'A'..'F']) do
      Inc(FTokenEnd);
    FTokLen := FTokenEnd - FTokenPos;
    FToken := copy(FLineText, FTokenPos, FTokLen);
  end
  else
  begin
    FTokenHash := HashToken(@FLineText[FTokenEnd], FTokLen);
    Inc(FTokenEnd, FTokLen);
    FToken := copy(FLineText, FTokenPos, FTokLen);
    if FLineText[FTokenEnd] = '(' then
      FTok := tkFunction
    else
      FTok := tkUndefined;
    if isEnd(FToken, 'if') then
      StartCodeFoldBlock(Pointer(IfBlockID))
    else if isEnd(FToken, 'while') then
      StartCodeFoldBlock(Pointer(WhileBlockID))
    else if isEnd(FToken, 'func') then
      StartCodeFoldBlock(Pointer(FuncBlockID))
    else if isEnd(FToken, 'for') then
      StartCodeFoldBlock(Pointer(ForBlockID))
    else if isEnd(FToken, 'do') then
      StartCodeFoldBlock(Pointer(UntilBlockID))
    else if isEnd(FToken, 'switch') then
      StartCodeFoldBlock(Pointer(SwitchBlockID))
    else if isEnd(FToken, 'select') then
      StartCodeFoldBlock(Pointer(SelectBlockID))
    else if isEnd(FToken, 'with') then
      StartCodeFoldBlock(Pointer(WithBlockID))
    else if isEnd(FToken, 'else') or isEnd(FToken, 'elseif') then
    begin
      EndCodeFoldBlock();
      StartCodeFoldBlock(Pointer(IfBlockID))
    end
    else if isEnd(FToken, 'endif') then
      EndCodeFoldBlock()
    else if isEnd(FToken, 'endfunc') then
      EndCodeFoldBlock()
    else if isEnd(FToken, 'wend') then
      EndCodeFoldBlock()
    else if isEnd(FToken, 'next') then
      EndCodeFoldBlock()
    else if isEnd(FToken, 'until') then
      EndCodeFoldBlock()
    else if isEnd(FToken, 'endselect') then
      EndCodeFoldBlock()
    else if isEnd(FToken, 'endswitch') then
      EndCodeFoldBlock()
    else if isEnd(FToken, 'endwith') then
      EndCodeFoldBlock();
  end;
end;

function Tau3SynHighlight.GetEol: boolean;
begin
  Result := FTokenPos > length(FLineText);
end;

procedure Tau3SynHighlight.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart := @FLineText[FTokenPos];
  TokenLength := FTokLen;
end;

function Tau3SynHighlight.GetTokenAttribute: TSynHighlighterAttributes;

  function KeyComp(aKey: string): boolean;
  var
    i: integer;
    t: string;
  begin
    {$IfDef CaseInsensitive}
    t := LowerCase(FToken);
    aKey := LowerCase(aKey);
    {$Else}
    t := FToken;
    {$EndIf}
    if Length(aKey) <> FTokLen then
    begin
      Result := False;
      Exit;
    end;
    Result := True;
    for i := 1 to FTokLen do
      if t[i] <> aKey[i] then
      begin
        Result := False;
        Break;
      end;
  end;

begin
  if FTok = tkUndefined then
    CheckHash;
  if (Length(FSelectText) > 0) and (KeyComp(FSelectText)) then
  begin
    Result := FSelectAttr;
    Result.Assign(GetAttr(FTok));
    Result.Background := $00DEDEDE;
    Result.FrameColor := clSilver;
    Result.FrameEdges := sfeAround;
  end
  else if FCurRange>0 then
    Result:=FCommentAttr
  else
    Result := GetAttr(FTok);
end;

function Tau3SynHighlight.GetRange: Pointer;
begin
  CodeFoldRange.RangeType := Pointer(PtrInt(FCurRange));
  Result := inherited GetRange;
end;

procedure Tau3SynHighlight.SetRange(Value: Pointer);
begin
  inherited SetRange(Value);
  FCurRange := PtrInt(CodeFoldRange.RangeType);
end;

procedure Tau3SynHighlight.ResetRange;
begin
  inherited ResetRange;
  FCurRange := 0;
end;

function Tau3SynHighlight.GetToken: string;
begin
  Result := FToken;
end;

function Tau3SynHighlight.GetTokenPos: integer;
begin
  Result := FTokenPos - 1;
end;

function Tau3SynHighlight.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttr;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttr;
    SYN_ATTR_KEYWORD: Result := FKeyAttr;
    SYN_ATTR_STRING: Result := FStrAttr;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttr;
    SYN_ATTR_SYMBOL: Result := FKeyAttr;
    else
      Result := nil;
  end;
end;

function Tau3SynHighlight.GetTokenKind: integer;
begin
  Result := Ord(FTok);
end;

destructor Tau3SynHighlight.Destroy;

  procedure FreeLst(l: TList);
  var
    i: integer;
  begin
    for i := 0 to l.Count - 1 do
      Dispose(PHashInfo(l[i]));
    l.Free;
  end;

var
  i: integer;
begin
  FDocAttr.Free;
  FStrAttr.Free;
  FCommentAttr.Free;
  FIdentifierAttr.Free;
  FKeyAttr.Free;
  FFunctionAttr.Free;
  FNumberAttr.Free;
  FSpaceAttr.Free;
  FTextAttr.Free;
  FVarAttr.Free;
  FSelectAttr.Free;
  FTempAttr.Free;
  for i := 0 to 255 do
    FreeLst(FHashList[i]);
  inherited;
end;

end.
