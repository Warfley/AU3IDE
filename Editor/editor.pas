unit Editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FileUtil, SynEdit, SynCompletion, Forms, Controls,
  au3Highlighter, Types, contnrs, LCLType, ExtCtrls, au3Types, UnitParser,
  Dialogs, Graphics, StdCtrls, Buttons, ComCtrls, strutils, CodeFormatter,
  ToolTip, ListRecords, SynEditTypes, Math, SynGutterBase, SynGutterChanges,
  GraphUtil, Project, gvector, fgl, TLStrings, LCLTranslator, Menus;

type

  { TEditorFrame }
  TAutoOpen = (aoNever, aoVar, aoAlways);

  TEditorFrame = class(TFrame)
    CodeExplorerPanel: TPanel;
    CodeExplorerHead: TPanel;
    CodeExplorerImages: TImageList;
    SearchButton: TButton;
    ReplaceButton: TButton;
    ReplaceAllButton: TButton;
    CodeEditor: TSynEdit;
    Completion: TSynCompletion;
    SearchEdit: TEdit;
    Label1: TLabel;
    SearchBar: TPanel;
    ReplaceEdit: TEdit;
    SelectHighlightTimer: TTimer;
    CheckSelTimer: TTimer;
    CloseSearchButton: TSpeedButton;
    CloseCodeExplorerButton: TSpeedButton;
    CESplitter: TSplitter;
    ToolTipTimer: TTimer;
    CodeExplorer: TTreeView;
    procedure CheckSelTimerTimer(Sender: TObject);
    procedure CloseCodeExplorerButtonClick(Sender: TObject);
    procedure CloseSearchButtonClick(Sender: TObject);
    procedure CodeEditorChange(Sender: TObject);
    procedure CodeEditorKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CodeEditorKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CodeEditorMouseLink(Sender: TObject; X, Y: integer;
      var AllowMouseLink: boolean);
    procedure CodeEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure CodeExplorerCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: boolean);
    procedure CodeExplorerDblClick(Sender: TObject);
    procedure CompletionCodeCompletion(var Value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure CompletionExecute(Sender: TObject);
    function CompletionPaintItem(const AKey: string; ACanvas: TCanvas;
      X, Y: integer; Selected: boolean; Index: integer): boolean;
    procedure CompletionSearchPosition(var APosition: integer);
    procedure ReplaceAllButtonClick(Sender: TObject);
    procedure ReplaceButtonClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure ReplaceEditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SearchEditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SelectHighlightTimerTimer(Sender: TObject);
    procedure ToolTipTimerTimer(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FIncludeFiles: TStringList;
    FIncludePath: string;
    FProject: Tau3Project;
    FOpenEditor: TOpenEditorEvent;
    topl: integer;
    FToolTip: TEditorToolTip;
    currFunc: string;
    currInfo: string;
    FOnParserFinished: TNotifyEvent;
    moveright: boolean;
    currWord: string;
    FOnChange: TNotifyEvent;
    Highlight: Tau3SynHighlight;
    FFunctions: TFuncList;
    FRequiredFiles: TStringList;
    FVars: TVarList;
    FCompVars: TStringList;
    FMacros: TStringList;
    FStdFunc: TFuncList;
    FFileName: string;
    FKeyWords: TStringList;
    FDefRanges: TObjectList;
    Parser: TUnitParser;
    FUseCount: TUseMap;
    FShowIncludeVars: boolean;
    FMaxCompletionCount: integer;
    FCompletionList: TStringList;
    FAutoOpen: TAutoOpen;
    function GetTemplatePos(UseCursor: boolean): TPoint;
    procedure LoadFuncList;
    procedure CompleteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    function GetCurrWord: string;
    function GetFont: TFont;
    procedure SetFont(f: TFont);
    procedure SetMaxCompletionCount(AValue: integer);
    procedure SetRanges(l: TObjectList);
    procedure SetFunc(l: TFuncList);
    procedure SetVar(l: TVarList);
    function GetAtPoint(p: TPoint): string;
    procedure ParserHasFinished(Sender: TObject);
    procedure LoadGeneralConf(FileName: string);
    procedure TTClick(Sender: TObject);
    { private declarations }
  public
    procedure ReLoadConf;
    procedure MoveHorz(i: IntPtr);
    procedure MoveVert(i: IntPtr);
    procedure SelectTemp(i: IntPtr);
    procedure ShowSearch;
    procedure SetFocus; override;
    procedure CodeJump(p: TPoint);
    procedure StartFormatter;
    procedure Save(p: string = '');
    procedure Load(p: string = '');
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property FunctionList: TFuncList read FFunctions write SetFunc;
    property VariableList: TVarList read FVars write SetVar;
    property FileName: string read FFileName write FFilename;
    property DefRanges: TObjectList read FDefRanges write SetRanges;
    property Font: TFont read GetFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnParserFinished: TNotifyEvent read FOnParserFinished
      write FOnParserFinished;
    property RequiredFiles: TStringList read FRequiredFiles;
    property OpenEditor: TOpenEditorEvent read FOpenEditor write FOpenEditor;
    property Highlighter: Tau3SynHighlight read Highlight;
    property ToolTip: TEditorToolTip read FToolTip;
    property Project: Tau3Project read FProject write FProject;
    property IncludePath: string read FIncludePath write FIncludePath;
    property UseCount: TUseMap read FUseCount;
    property MaxCompletionCount: integer read FMaxCompletionCount
      write SetMaxCompletionCount;
    property ShowIncludeVars: boolean read FShowIncludeVars write FShowIncludeVars;
    property AutoOpen: TAutoOpen read FAutoOpen write FAutoOpen;
    { public declarations }
  end;

implementation

{$R *.lfm}

function IncludeContainsFile(Filename: string; i: string): boolean;
begin
  if isEnd(i, '#include') then
    Delete(i, 1, Pos('<', i));
  Filename := LowerCase(Filename);
  i := LowerCase(i);
  Result := (Pos(i, Filename) >= 1) or (Length(i) = 0);
end;

procedure TEditorFrame.LoadGeneralConf(FileName: string);
var
  conf: TEditorConfig;
  f: file of TEditorConfig;
  i: integer;
  fd: TFontData;
begin
  AssignFile(f, FileName);
  try
    Reset(f);
    Read(f, conf);
  finally
    CloseFile(f);
  end;
  with conf do
  begin
    if CERight then
    begin
      CodeExplorerPanel.Align := alRight;
      CESplitter.Align := alRight;
    end
    else
    begin
      CodeExplorerPanel.Align := alLeft;
      CESplitter.Align := alLeft;
    end;
    Self.Color := BGCol;
    CodeEditor.Color := EditBGCol;
    CodeExplorer.BackgroundColor := EditBGCol;
    SearchEdit.Color := EditBGCol;
    ReplaceEdit.Color := EditBGCol;
    with CodeEditor.Gutter do
    begin
      Color := GutterCol;
      for i := 0 to Parts.Count - 1 do
      begin
        if Parts[i] is TSynGutterPartBase then
        begin
          (Parts[i] as TSynGutterPartBase).MarkupInfo.Background :=
            GutterCol;
          (Parts[i] as TSynGutterPartBase).MarkupInfo.Foreground :=
            GutterFore;
        end;
        if Parts[i] is TSynGutterChanges then
        begin
          (Parts[i] as TSynGutterChanges).ModifiedColor :=
            GutterEdited;
          (Parts[i] as TSynGutterChanges).SavedColor :=
            GutterSaved;
        end;
      end;
      CodeEditor.SelectedColor.Background := SelCol;
      CodeEditor.SelectedColor.Foreground := SelFCol;
      Font.Color := TextColor;
      SearchBar.Font.Color := TextColor;
      CodeExplorer.Color := TextColor;
      CodeExplorer.ExpandSignColor := GetHighLightColor(TextColor);
      CodeExplorer.TreeLineColor := GetHighLightColor(TextColor);
      CodeExplorer.SeparatorColor := GetHighLightColor(TextColor);
      CodeExplorer.Invalidate;
      if PastEOL then
        CodeEditor.Options :=
          CodeEditor.Options + [eoScrollPastEol]
      else
        CodeEditor.Options :=
          CodeEditor.Options - [eoScrollPastEol];
      if CaretAV then
        CodeEditor.Options2 :=
          CodeEditor.Options2 + [eoAlwaysVisibleCaret]
      else
        CodeEditor.Options2 :=
          CodeEditor.Options2 - [eoAlwaysVisibleCaret];
      CodeEditor.TabWidth := TabWidth;
      FToolTip.Color := TTipColor;
      FToolTip.Font.Color := TTipFont;
      CodeEditor.Font.Name := FontName;
      fd := CodeEditor.Font.FontData;
      with fd do
      begin
        Height := EditorFont.Height;
        Pitch := EditorFont.Pitch;
        Style := EditorFont.Style;
        CharSet := EditorFont.CharSet;
        Quality := EditorFont.Quality;
        Name := EditorFont.Name;
        Orientation := EditorFont.Orientation;
      end;
      CodeEditor.Font.FontData := fd;
    end;
  end;
end;

procedure TEditorFrame.ReLoadConf;
begin
  LoadGeneralConf(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'editor.cfg');
  Highlight.LoadConfig(IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0))) + 'HL');
  FKeyWords.Clear;
  FKeyWords.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'Keywords.lst');
  FStdFunc.Clear;
  LoadFuncList;
  FMacros.Clear;
  FCompVars.Clear;
  FMacros.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'Macros.lst');
  FCompVars.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'STDVars.lst');
end;

procedure TEditorFrame.MoveHorz(i: IntPtr);
var
  p: TPoint;
begin
  p := CodeEditor.LogicalCaretXY;
  p.x := p.x + i;
  while Length(CodeEditor.Lines[p.y - 1]) < p.x - 1 do
    CodeEditor.Lines[p.y - 1] := CodeEditor.Lines[p.y - 1] + ' ';
  CodeEditor.LogicalCaretXY := p;
end;

procedure TEditorFrame.MoveVert(i: IntPtr);
var
  p: TPoint;
begin
  p := CodeEditor.LogicalCaretXY;
  p.y := p.y + i;
  while CodeEditor.Lines.Count < p.y do
    CodeEditor.TextBetweenPoints[Point(
      Length(CodeEditor.Lines[CodeEditor.Lines.Count - 1]), CodeEditor.Lines.Count),
      Point(Length(CodeEditor.Lines[CodeEditor.Lines.Count - 1]),
      CodeEditor.Lines.Count)] := '#13';
  CodeEditor.LogicalCaretXY := p;
end;

procedure TEditorFrame.TTClick(Sender: TObject);
begin
  CodeEditor.LogicalCaretXY :=
    CodeEditor.PixelsToLogicalPos(CodeEditor.ScreenToClient(Mouse.CursorPos));
  CheckSelTimerTimer(CheckSelTimer);
  CodeEditorMouseUp(CodeEditor, mbLeft, [ssLeft],
    CodeEditor.ScreenToClient(Mouse.CursorPos).x,
    CodeEditor.ScreenToClient(Mouse.CursorPos).y);
end;

function TEditorFrame.GetTemplatePos(UseCursor: boolean): TPoint;
var
  i, l, p: integer;
  ln: string;
  inStr: boolean;
begin
  Result.x := 0;
  Result.y := 0;
  p := CodeEditor.LogicalCaretXY.x - 1;
  ln := CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 1];
  l := Length(ln);
  inStr := False;
  for i := 1 to l do
  begin
    if ln[i] = '"' then
      inStr := not inStr
    else if not inStr then
      if Result.x = 0 then
      begin
        if (i > p + 1) and UseCursor then
          Break;
        if ln[i] = '{' then
          Result.x := i;
      end
      else
      begin
        if ln[i] = '{' then
        begin
          if (i > p) and UseCursor then
            Result.x := 0
          else
            Result.x := i;
        end
        else if ln[i] = '}' then
        begin
          if (i >= p) or not UseCursor then
          begin
            Result.y := i + 1;
            Break;
          end
          else
            Result.x := 0;
        end;
      end;
  end;
  if Result.y = 0 then
    Result.x := 0;
end;

procedure TEditorFrame.SelectTemp(i: IntPtr);
var
  p: TPoint;
begin
  p := GetTemplatePos(False);
  if p.x > 0 then
  begin
    CodeEditor.BlockBegin := Point(p.x, CodeEditor.LogicalCaretXY.Y);
    CodeEditor.BlockEnd := Point(p.y, CodeEditor.LogicalCaretXY.Y);
    CodeEditor.LogicalCaretXY := Point(p.y, CodeEditor.LogicalCaretXY.y);
  end;
end;

function TEditorFrame.GetFont: TFont;
begin
  Result := CodeEditor.Font;
end;

procedure TEditorFrame.SetFont(f: TFont);
begin
  CodeEditor.Font := f;
end;

procedure TEditorFrame.SetMaxCompletionCount(AValue: integer);
begin
  if FMaxCompletionCount = AValue then
    Exit;
  while FUseCount.Count > AValue do
    FUseCount.Delete(FUseCount.Count - 1);
  FMaxCompletionCount := AValue;
end;

procedure TEditorFrame.SetRanges(l: TObjectList);
var
  i: integer;
begin
  for i := 0 to FDefRanges.Count - 1 do
    FDefRanges[i].Free;
  FDefRanges.Clear;
  for i := 0 to l.Count - 1 do
    FDefRanges.Add(l[i]);
end;


procedure TEditorFrame.ShowSearch;
begin
  SearchBar.Show;
  SearchEdit.SetFocus;
end;

procedure TEditorFrame.LoadFuncList;
var
  sl1, sl2: TStringList;
  i: integer;
begin
  sl1 := TStringList.Create;
  sl2 := TStringList.Create;
  try
    sl1.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'Funcs.lst');
    for i := 0 to sl1.Count - 1 do
      if sl1[i][1] = ';' then
        sl2.Add(Copy(sl1[i], 2, Length(sl1[i])))
      else
      begin
        FStdFunc.Add(FuncInfo(sl1[i], -1, sl2.Text));
        sl2.Clear;
      end;
  finally
    sl1.Free;
    sl2.Free;
  end;
end;

constructor TEditorFrame.Create(TheOwner: TComponent);
begin
  inherited;
  CodeExplorer.Items.Clear;
  with CodeExplorer.Items.Add(nil, SInclude) do
  begin
    ImageIndex:=0;
    SelectedIndex:=0;
  end;
  with CodeExplorer.Items.Add(nil, SVariables) do
  begin
    ImageIndex:=0;
    SelectedIndex:=0;
  end;
  with CodeExplorer.Items.Add(nil, SFunctions) do
  begin
    ImageIndex:=0;
    SelectedIndex:=0;
  end;
  currInfo := '';
  Completion.OnKeyDown := @CompleteKeyDown;
  CodeEditor.Lines.Add('');
  FOnChange := nil;
  FToolTip := TEditorToolTip.Create(self);
  FToolTip.Parent := CodeEditor;
  moveright := True;
  Parser := TUnitParser.Create(True);
  Highlight := Tau3SynHighlight.Create(nil);
  FToolTip.Highlighter := Highlight;
  CodeEditor.Highlighter := Highlight;
  FFunctions := TFuncList.Create;
  FVars := TVarList.Create;
  FStdFunc := TFuncList.Create;
  FKeyWords := TStringList.Create;
  FDefRanges := TObjectList.Create(False);
  FRequiredFiles := TStringList.Create;
  FCompVars := TStringList.Create;
  FMacros := TStringList.Create;
  ReLoadConf;
  UpdateTimerTimer(nil);
  FIncludeFiles := TStringList.Create;
  FToolTip.OnClick := @TTClick;
  currWord := '';
  FUseCount := TUseMap.Create;
  FCompletionList := TStringList.Create;
  FMaxCompletionCount := 32;
  FAutoOpen := aoVar;
  FShowIncludeVars := True;
end;

procedure TEditorFrame.SetFocus;
begin
  inherited;
  try
  CodeEditor.SetFocus;
  except
    on E: EInvalidOperation do ;
  end;
end;

procedure TEditorFrame.ParserHasFinished(Sender: TObject);
var
  i: integer;
  ni, nv, nf, tmp: TTreeNode;
begin
  if CodeExplorer.Visible then
  begin
    CodeExplorer.BeginUpdate;
    try
      for i := 0 to CodeExplorer.Items.Count - 1 do
        if CodeExplorer.Items[i].ImageIndex > 0 then
          CodeExplorer.Items[i].ImageIndex := -1;
      ni := CodeExplorer.Items.FindNodeWithText(SInclude);
      nf := CodeExplorer.Items.FindNodeWithText(SFunctions);
      nv := CodeExplorer.Items.FindNodeWithText(SVariables);
      i := 0;
      for i := 0 to FRequiredFiles.Count - 1 do
        if FileExistsUTF8(GetFullPath(FRequiredFiles[i], FIncludePath,
          ExtractFilePath(FFileName), FProject.Paths)) then
        begin
          tmp := CodeExplorer.Items.FindNodeWithText(FRequiredFiles[i]);
          if not Assigned(tmp) then
            tmp := CodeExplorer.Items.AddChild(ni, FRequiredFiles[i]);
          with tmp do
          begin
            ImageIndex := 1;
            SelectedIndex := 1;
            Data := Pointer(i);
          end;
        end;
      for i := 0 to FFunctions.Count - 1 do
      begin
        tmp := CodeExplorer.Items.FindNodeWithText(FFunctions[i].Name);
        if not Assigned(tmp) then
          tmp := CodeExplorer.Items.AddChild(nf, FFunctions[i].Name);
        with tmp do
        begin
          ImageIndex := 2;
          SelectedIndex := 2;
          Data := Pointer(i);
        end;
      end;
      for i := 0 to FVars.Count - 1 do
      begin
        tmp := CodeExplorer.Items.FindNodeWithText(FVars[i].Name);
        if not Assigned(tmp) then
          tmp := CodeExplorer.Items.AddChild(nv, FVars[i].Name);
        with tmp do
        begin
          ImageIndex := 3;
          SelectedIndex := 3;
          Data := Pointer(i);
        end;
      end;
      i := 0;
      while i < CodeExplorer.Items.Count do
        if CodeExplorer.Items[i].ImageIndex = -1 then
          CodeExplorer.Items[i].Free
        else
          Inc(i);
    finally
      CodeExplorer.EndUpdate;
    end;
  end;
  if FShowIncludeVars and Assigned(FOnParserFinished) then
    FOnParserFinished(Self);
end;

procedure TEditorFrame.CompletionExecute(Sender: TObject);

  procedure InsertSorted(l: TStringList; v: string);
  var
    i, x, p: integer;
  begin
    if not FUseCount.Find(LowerCase(v), p) then
      l.Add(v)
    else
    begin
      for i := 0 to l.Count - 1 do
        if not (FUseCount.Find(LowerCase(l[i]), x) and (x >= p)) then
        begin
          l.Insert(i, v);
          Exit;
        end;
      l.Add(v);
    end;
  end;

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

var
  i, x: integer;
  sl: TStringList;
begin
  if isEnd(CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 1], '#include') then
  begin
    Completion.ItemList.Clear;
    FIncludeFiles.Clear;
    FIncludeFiles.Duplicates := dupIgnore;
    sl := TStringList.Create;
    try
      FindAllFiles(sl, ExtractFilePath(FFileName), '*.au3');
      FIncludeFiles.AddStrings(sl);
      sl.Clear;
      FindAllFiles(sl, ExtractFilePath(FFileName), '*.afm');
      FIncludeFiles.AddStrings(sl);
      sl.Clear;
      FindAllFiles(sl, IncludePath, '*.au3');
      FIncludeFiles.AddStrings(sl);
      sl.Clear;
      for i := 0 to FProject.Paths.Count - 1 do
      begin
        FindAllFiles(sl, FProject.Paths[i], '*.au3');
        FIncludeFiles.AddStrings(sl);
        sl.Clear;
        FindAllFiles(sl, FProject.Paths[i], '*.afm');
        FIncludeFiles.AddStrings(sl);
        sl.Clear;
      end;
    finally
      sl.Free;
    end;
    for i := 0 to FIncludeFiles.Count - 1 do
      FIncludeFiles[i] := GetRelInclude(FIncludeFiles[i], IncludePath,
        ExtractFilePath(FFileName), FProject.Paths);
    for i := 0 to FIncludeFiles.Count - 1 do
      if IncludeContainsFile(FIncludeFiles[i], Completion.CurrentString) then
        Completion.ItemList.Add(FIncludeFiles[i]);
    Exit;
  end;
  // Fill Completion List
  FCompletionList.Clear;
  FCompletionList.Duplicates := dupIgnore;

  // Keywords
  for i := 0 to FKeyWords.Count - 1 do
    InsertSorted(FCompletionList, FKeyWords[i]);

  // Macros
  for i := 0 to FMacros.Count - 1 do
    InsertSorted(FCompletionList, FMacros[i]);

  // STD Func
  for i := 0 to FStdFunc.Count - 1 do
    InsertSorted(FCompletionList, FStdFunc[i].Name);

  // Func
  for i := 0 to FFunctions.Count - 1 do
    InsertSorted(FCompletionList, FFunctions[i].Name);

  // Compiler Vars
  for i := 0 to FCompVars.Count - 1 do
    InsertSorted(FCompletionList, FCompVars[i]);

  // Global Vars
  for i := 0 to FVars.Count - 1 do
    if (FVars[i].Line <= CodeEditor.LogicalCaretXY.y - 1) or
      (FVars[i].FileName <> '') then
      InsertSorted(FCompletionList, FVars[i].Name);

  // Local Vars
  for x := 0 to FDefRanges.Count - 1 do
    if (CodeEditor.LogicalCaretXY.y - 1 >= (FDefRanges[x] as TDefRange).StartLine) and
      (CodeEditor.LogicalCaretXY.y - 1 < (FDefRanges[x] as TDefRange).EndLine) then
      for i := 0 to (FDefRanges[x] as TDefRange).Vars.Count - 1 do
        if ((FDefRanges[x] as TDefRange).Vars[i].Line <=
          CodeEditor.LogicalCaretXY.y - 1) then
          InsertSorted(FCompletionList, (FDefRanges[x] as TDefRange).Vars[i].Name);
  // Filter
  Completion.ItemList.Clear;
  if Length(Completion.CurrentString) = 0 then
    Completion.ItemList.AddStrings(FCompletionList)
  else
    for i := 0 to FCompletionList.Count - 1 do
      if Pos(LowerCase(Completion.CurrentString), LowerCase(FCompletionList[i])) >= 1 then
        if Length(Completion.CurrentString) = Length(FCompletionList[i]) then
          Completion.ItemList.Insert(0, FCompletionList[i])
        else
          Completion.ItemList.Add(FCompletionList[i]);

  if Completion.ItemList.Count = 0 then
    Completion.ItemList.Add(Completion.CurrentString);
  Completion.ItemList.Add('');
  Completion.Position := 0;
end;

function TEditorFrame.CompletionPaintItem(const AKey: string;
  ACanvas: TCanvas; X, Y: integer; Selected: boolean; Index: integer): boolean;
var
  s, l, p, currStart, currEnd, tokCurrStart, tokCurrEnd: integer;
  curr: TTokenType;
  tok, t: string;
begin
  currStart:=Pos(LowerCase(Completion.CurrentString), LowerCase(AKey));
  currEnd:=currStart+Length(Completion.CurrentString);
  with ACanvas do
  begin
    p := 0;
    if Length(AKey) > 0 then
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
          Inc(l)
        else if (curr = tkUnknown) and (s + l <= Length(AKey)) and
          (AKey[s + l] = '(') then
          curr := tkFunction;
        tok := Copy(AKey, s, l);
        Font.Bold := False;
        case curr of
          tkVar:
            Font.Color := Highlight.VariableAttribute.Foreground;
          tkFunction:
            Font.Color := Highlight.FunctionAttribute.Foreground;
          tkUnknown:
            Font.Color := Highlight.NumberAttribute.Foreground;
          tkNumber:
            Font.Color := Highlight.NumberAttribute.Foreground;
          tkString:
            Font.Color := Highlight.StringAttribute.Foreground;
          tkSpace:
            Font.Color := Highlight.SpaceAttribute.Foreground;
          tkSymbol:
            Font.Color := Highlight.SymbolAttribute.Foreground;
          tkTemp:
            Font.Color := Highlight.TempAttribute.Foreground;
        end;
        if (Length(AKey) > 0) and (currStart>0) and
          (s < currEnd) and (s+l > currStart) then
        begin
          tokCurrStart:=max(0, currStart - s);
          tokCurrEnd:=Min(Length(tok), currEnd-s);
          t:=Copy(tok, 1, tokCurrStart);
          TextOut(x + p, y, t);
          inc(p, TextWidth(t));
          font.Bold:=True;
          t:=Copy(tok, tokCurrStart+1, tokCurrEnd-tokCurrStart);
          TextOut(x + p, y, t);
          inc(p, TextWidth(t));
          font.Bold:=false;
          t:=Copy(tok, tokCurrEnd+1, Length(tok)-tokCurrEnd);
          TextOut(x + p, y, t);
          inc(p, TextWidth(t)+2);
        end
        else
        begin
          TextOut(x + p, y, tok);
          Inc(p, TextWidth(tok)+2);
        end;
        Inc(s, l);
      end;
    end;
  end;
  Result := True;
end;

procedure TEditorFrame.Save(p: string = '');
begin
  if (p = '') and (FFileName <> '') then
    p := FFileName;
  if (p <> '') then
  begin
    CodeEditor.Lines.SaveToFile(p);
    CodeEditor.MarkTextAsSaved;
    FFileName := p;
  end;
end;

procedure TEditorFrame.Load(p: string = '');
begin
  if (p = '') and (FFileName <> '') then
    p := FFileName;
  if (p <> '') then
  begin
    CodeEditor.Lines.LoadFromFile(p);
    FFileName := p;
    UpdateTimerTimer(nil);
  end;
end;

procedure TEditorFrame.CompleteKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key = 8) and ((Length(Completion.CurrentString) = 0) or
    ((Length(Completion.CurrentString) > 0) and
    (Completion.CurrentString[1] in ['$', '@']))) then
    Completion.Deactivate
    {$IfDef Windows}
   else if (Key>32) and (Length(GetCharFromVKey(Key))>0) and not (GetCharFromVKey(Key)[1] in ['A'..'Z', 'a'..'z', '0'..'9']) then
   begin
    CodeEditor.InsertTextAtCaret(GetCharFromVKey(Key));
    Key := 0;
    Completion.Deactivate;
   end
    {$Else}
  else if (Key=188) and (Shift=[]) then
  begin
    Key := 0;
    CodeEditor.InsertTextAtCaret(',');
    Completion.Deactivate;
  end
  else if Key in [17, 18, 188] then
    Completion.Deactivate
  {$EndIf}
  else if Key = 32 then
  begin
    Key := 0;
    CodeEditor.InsertTextAtCaret(' ');
    Completion.Deactivate;
  end
  else if key = 9 then
    key := 13;
end;

procedure TEditorFrame.CompletionSearchPosition(var APosition: integer);

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

var
  i: integer;
begin
  Completion.ItemList.Clear;
  if isEnd(CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 1], '#include') then
  begin
    for i := 0 to FIncludeFiles.Count - 1 do
      if IncludeContainsFile(FIncludeFiles[i], Completion.CurrentString) then
        Completion.ItemList.Add(FIncludeFiles[i]);
    if Completion.ItemList.Count = 0 then
      Completion.ItemList.Add(Copy(Completion.CurrentString,
        pos('<', Completion.CurrentString) + 1, Length(Completion.CurrentString)));
  end
  else
  begin
    Completion.ItemList.Clear;
    if Length(Completion.CurrentString) = 0 then
      Completion.ItemList.AddStrings(FCompletionList)
    else
      for i := 0 to FCompletionList.Count - 1 do
        if Pos(LowerCase(Completion.CurrentString),
          LowerCase(FCompletionList[i])) >= 1 then
          if Length(Completion.CurrentString) = Length(FCompletionList[i]) then
            Completion.ItemList.Insert(0, FCompletionList[i])
          else
            Completion.ItemList.Add(FCompletionList[i]);
  end;

  if Completion.ItemList.Count = 0 then
    Completion.ItemList.Add(Completion.CurrentString);
  Completion.ItemList.Add('');
  Completion.Position := min(Max(Completion.Position, 0), Completion.ItemList.Count - 1);
end;

procedure TEditorFrame.ReplaceAllButtonClick(Sender: TObject);
begin
  if CodeEditor.SearchReplaceEx(SearchEdit.Text, '', [ssoReplace, ssoReplaceAll],
    Point(0, 0)) = 0 then
    ShowMessage(SNoMatchFound);
end;

procedure TEditorFrame.ReplaceButtonClick(Sender: TObject);
begin
  if CodeEditor.SearchReplaceEx(SearchEdit.Text, '', [ssoReplace],
    CodeEditor.BlockEnd) = 0 then
    if MessageDlg(SEOFReachedTitle, SEOFReachedText,
      mtConfirmation, mbYesNo, SEOFReachedKeyword) = mrYes then
      if CodeEditor.SearchReplaceEx(SearchEdit.Text, '', [ssoReplace],
        Point(0, 0)) = 0 then
        ShowMessage(SNoMatchFound);
end;

procedure TEditorFrame.SearchButtonClick(Sender: TObject);
begin
  if CodeEditor.SearchReplaceEx(SearchEdit.Text, '', [], CodeEditor.BlockEnd) = 0 then
    if MessageDlg(SEOFReachedTitle,
      SEOFReachedText,
      mtConfirmation, mbYesNo, SEOFReachedKeyword) = mrYes then
      if CodeEditor.SearchReplaceEx(SearchEdit.Text, '', [], Point(0, 0)) = 0 then
        ShowMessage(SNoMatchFound);
end;

procedure TEditorFrame.ReplaceEditKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = 13 then
    ReplaceButtonClick(nil);
end;

procedure TEditorFrame.SearchEditKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = 13 then
    SearchButtonClick(nil);
end;

procedure TEditorFrame.SelectHighlightTimerTimer(Sender: TObject);
begin
  SelectHighlightTimer.Enabled := False;
  Highlight.SelectedText := currWord;
  CodeEditor.Invalidate;
end;

procedure TEditorFrame.ToolTipTimerTimer(Sender: TObject);
begin
  ToolTipTimer.Enabled := False;
  FToolTip.Info := currInfo;
  FToolTip.Func := currFunc;
  FToolTip.ShowAt(CodeEditor.CaretXPix, CodeEditor.CaretYPix, CodeEditor.LineHeight);
  FToolTip.BringToFront;
end;

procedure TEditorFrame.UpdateTimerTimer(Sender: TObject);
begin
  if Trim(CodeEditor.Lines.Text) = '' then
    Exit;
  if Parser.Finished or Parser.Suspended then
  begin
    Parser.Free;
    Parser := TUnitParser.Create(True);
    Parser.OnFinished := @ParserHasFinished;
    Parser.Text := CodeEditor.Lines.Text;
    Parser.Funcs := FFunctions;
    Parser.Vars := FVars;
    Parser.RequiredFiles := FRequiredFiles;
    Parser.Ranges := FDefRanges;
    Parser.UseCount := UseCount;
    Parser.Start;
  end;
end;

procedure TEditorFrame.CompletionCodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
var
  p, x: integer;
  ln: string;
begin
  if FUseCount.Find(LowerCase(Value), p) then
  begin
    x := FUseCount.Data[p];
    FUseCount.Delete(p);
    FUseCount.InsertKeyData(0, LowerCase(Value), x + 1);
  end
  else if FUseCount.Count < MaxCompletionCount then
    FUseCount.Add(LowerCase(Value), 1)
  else
  begin
    // Delete Least important
    for x := MaxCompletionCount - 1 downto MaxCompletionCount div 2 do
      if FUseCount.Data[x + 1] < FUseCount.Data[x] * 2 then
      begin
        FUseCount.Delete(x + 1);
        Break;
      end;
    if FUseCount.Count = MaxCompletionCount then
      FUseCount.Delete(MaxCompletionCount div 2);
    FUseCount.Add(LowerCase(Value), 1);
  end;
  ln := CodeEditor.Lines[SourceStart.y - 1];
  moveright := False;
  if Length(Value) = 0 then
    Value := Completion.CurrentString
  else if (Value[1] = '$') then
  begin
    if Value[Length(Value)] = ']' then
      Application.QueueAsyncCall(@MoveHorz, -1)
    else if isEnd(ln, 'dim') and AnsiEndsStr(Value, Trim(ln)) then
    begin
      Value := Value + '[]';
      Application.QueueAsyncCall(@MoveHorz, -1);
    end
    else if Completion.CurrentString = Trim(ln) then
      Value := Value + ' = '
    else if (SourceEnd.x = SourceStart.x + Length(Completion.CurrentString)) and
      not isEnd(ln, 'global') then
      Value := Value;
  end
  else
  begin
    p := Pos('(', Value);
    if p > 0 then
    begin
      if Value[p + 1] <> ')' then
        Application.QueueAsyncCall(@MoveHorz, -1);
      if PosEx('{', Value, p + 1) = 0 then
      begin
        SetLength(Value, Pos('(', Value));
        Value := Value + ')';
      end;
    end
    else if isEnd(ln, 'func') and AnsiEndsStr(Value, TrimRight(ln)) then
    begin
      Value := Value + '()';
      Application.QueueAsyncCall(@MoveHorz, -1);
    end;
  end;
  if (Length(Value) > 0) and (Value[1] in ['$', '@'])
    and (SourceStart.x>1) and // there is at least one char in front of
    (ln[SourceStart.x-1] in ['$', '@']) // and its a special char
    then
    Value := Copy(Value, 2, Length(Value) - 1);
  if (Pos('{', Value) > 0) then
    Application.QueueAsyncCall(@SelectTemp, SourceStart.x);
  (*rpval := Value;
  Value := Copy(ln, SourceStart.x, PosEx(Completion.CurrentString, ln, SourceStart.x))+
    Value+
    Copy(ln, PosEx(Completion.CurrentString, ln, SourceStart.x)+Length(Completion.CurrentString)+1,
    SourceEnd.x-PosEx(Completion.CurrentString, ln, SourceStart.x)+Length(Completion.CurrentString));
  Application.QueueAsyncCall(@MoveHorz, -(Length(Value) -
    (Pos(rpval, Value) + Length(rpval) - 1)));*)
end;

procedure TEditorFrame.CodeEditorKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);

  function GotClosed(i: integer; sTok, eTok: string): boolean;
  var
    counter, c: integer;
  begin
    while (i >= 0) do
      if isEnd(CodeEditor.Lines[i], 'func') or isEnd(CodeEditor.Lines[i], 'endfunc') then
      begin
        Inc(i);
        Break;
      end
      else
        Dec(i);
    c := CodeEditor.Lines.Count;
    counter := 0;
    while (i < c) and (not (isEnd(LowerCase(CodeEditor.Lines[i]), 'endfunc'))) and
      (not (isEnd(LowerCase(CodeEditor.Lines[i]), 'func'))) do
    begin
      if isEnd(LowerCase(CodeEditor.Lines[i]), sTok) then
        Inc(counter)
      else if isEnd(LowerCase(CodeEditor.Lines[i]), eTok) then
        Dec(counter);
      Inc(i);
    end;
    Result := counter <= 0;
  end;

var
  ln, pref, tmp: string;
  i, x, l, cx, cy: integer;
  b: boolean;
  p: TPoint;
begin
  if (Key = 13) and moveright then
  begin
    ln := LowerCase(CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 2]);
    i := 1;
    l := Length(ln);
    while (i <= l) and (ln[i] in [#0..#32]) do
      Inc(i);
    pref := Copy(ln, 1, i - 1);
    i := CodeEditor.LogicalCaretXY.y - 2;
    if isEnd(ln, 'while') then
    begin
      if not GotClosed(i, 'while', 'wend') then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'WEnd';
      end;
      Application.QueueAsyncCall(@MoveHorz, 2);
    end
    else if isEnd(ln, 'for') then
    begin
      if not GotClosed(i, 'for', 'next') then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'Next';
      end;
      Application.QueueAsyncCall(@MoveHorz, 2);
    end
    else if isEnd(ln, 'if') then
    begin
      if not GotClosed(i, 'if', 'endif') then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'EndIf';
      end;
      Application.QueueAsyncCall(@MoveHorz, 2);
    end
    else if isEnd(ln, '#cs') then
    begin
      if not GotClosed(i, '#cs', '#ce') then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + '#ce';
      end;
      Application.QueueAsyncCall(@MoveHorz, 2);
    end
    else if isEnd(ln, 'with') then
    begin
      if not GotClosed(i, 'with', 'endwith') then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'EndWith';
      end;
      Application.QueueAsyncCall(@MoveHorz, 2);
    end
    else if isEnd(ln, 'switch') then
    begin
      if not GotClosed(i, 'switch', 'endswitch') then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'EndSwitch';
      end;
      CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
        pref + '  Case {Value}';
      Application.QueueAsyncCall(@SelectTemp, 0);
    end
    else if isEnd(ln, 'select') then
    begin
      if not GotClosed(i, 'select', 'endselect') then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'EndSelect';
      end;
      CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
        pref + '  Case ';
      Application.QueueAsyncCall(@MoveHorz, 7);
    end
    else if isEnd(ln, 'case') then
      Application.QueueAsyncCall(@MoveHorz, 2)
    else if isEnd(ln, 'do') then
    begin
      if not GotClosed(i, 'do', 'until') then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'Until <Condition>';
      end;
      Application.QueueAsyncCall(@MoveHorz, 2);
    end
    else if isEnd(ln, 'func') then
    begin
      b := True;
      for x := i + 1 to CodeEditor.Lines.Count - 1 do
        if isEnd(CodeEditor.Lines[x], 'func') then
          break
        else if isEnd(CodeEditor.Lines[x], 'endfunc') then
        begin
          b := False;
          break;
        end;
      if b then
      begin
        CodeEditor.TextBetweenPoints[Point(0, i + 2), Point(0, i + 2)] :=
          #13 + pref + 'EndFunc';
      end;
      Application.QueueAsyncCall(@MoveHorz, 2);
    end;
  end
  else if (Key = Ord('D')) and (ssCtrl in Shift) then
    StartFormatter
  else if (Key = Ord('F')) and (ssCtrl in Shift) then
    ShowSearch;
  moveright := True;
  if (key in [Ord('A')..Ord('Z'), Ord('0')..Ord('9')]) or
    ((key = 8) and (AutoOpen = aoAlways)) then
  begin
    tmp := GetCurrWord;
    p := Point(CodeEditor.CaretXPix, CodeEditor.CaretYPix + CodeEditor.LineHeight);
    p := CodeEditor.ClientToScreen(p);
    cx := CodeEditor.LogicalCaretXY.x;
    cy := CodeEditor.LogicalCaretXY.y;
    if (Length(tmp) >= 1) and not (ssCtrl in Shift) and not
      (ssAlt in Shift) and ((AutoOpen = aoAlways) or
      ((AutoOpen = aoVar) and (tmp[1] in ['$', '#', '@']) and not
      ((cx>Length(CodeEditor.Lines[cy-1])) and (CodeEditor.Lines[Cy - 1][Cx] in ['$', '#', '@'])))) then
      Completion.Execute(GetCurrWord, p);
  end
  else if Key in [VK_LEFT..VK_DOWN] then
  begin
    p := GetTemplatePos(True);
    if p.x > 0 then
    begin
      if (CodeEditor.LogicalCaretXY.x = p.y - 1) and (Key = VK_LEFT) then
      begin
        CodeEditor.LogicalCaretXY := Point(p.x, CodeEditor.LogicalCaretXY.y);
        CodeEditor.BlockBegin := CodeEditor.LogicalCaretXY;
        CodeEditor.BlockEnd := CodeEditor.LogicalCaretXY;
        Exit;
      end;
      CodeEditor.BlockBegin := Point(p.x, CodeEditor.LogicalCaretXY.Y);
      CodeEditor.BlockEnd := Point(p.y, CodeEditor.LogicalCaretXY.Y);
      CodeEditor.LogicalCaretXY := Point(p.y, CodeEditor.LogicalCaretXY.y);
    end;
  end;
end;

procedure TEditorFrame.CodeEditorMouseLink(Sender: TObject; X, Y: integer;
  var AllowMouseLink: boolean);

  function GetCurrFunc(sel: string; out f: TFuncInfo): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FFunctions.Count - 1 do
      if LowerCase(Copy(FFunctions[i].Name, 1, Pos('(', FFunctions[i].Name) - 1)) =
        LowerCase(sel) then
      begin
        Result := True;
        f := FFunctions[i];
        if f.FileName = '' then
          exit;
      end;
  end;

  function GetCurrVar(sel: string; out v: TVarInfo): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FVars.Count - 1 do
    begin
      if isEnd(FVars[i].Name, sel) then
      begin
        Result := True;
        v := FVars[i];
        if v.FileName = '' then
          exit;
      end;
    end;
  end;

  function GetCurrInclude(out s: string): boolean;
  var
    ln: string;
  begin
    Result := False;
    ln := CodeEditor.Lines[CodeEditor.PixelsToLogicalPos(Point(x, y)).y - 1];
    if isEnd(ln, '#include') then
    begin
      if pos('<', ln) > 0 then
        s := Trim(ExtractBetween(ln, '<', '>'))
      else if pos('"', ln) > 0 then
        s := Trim(ExtractBetween(ln, '"', '"'));
    end;
    Result := Length(s) > 0;
  end;

var
  l, i, n: integer;
  sel: string;
  f: TFuncInfo;
  v: TVarInfo;
begin
  if (CodeEditor.Lines.Count < CodeEditor.LinesInWindow) and
    (y > CodeEditor.Lines.Count * CodeEditor.LineHeight) then
  begin
    AllowMouseLink := False;
    Exit;
  end;
  l := y - 1;
  sel := GetAtPoint(Point(X, Y));
  if GetCurrFunc(sel, f) or GetCurrVar(sel, v) or GetCurrInclude(sel) then
  begin
    AllowMouseLink := True;
    Exit;
  end
  else
    for n := 0 to FDefRanges.Count - 1 do
      for i := 0 to (FDefRanges[n] as TDefRange).Vars.Count - 1 do
        if (l >= (FDefRanges[n] as TDefRange).StartLine) and
          (l <= (FDefRanges[n] as TDefRange).EndLine) and
          ((FDefRanges[n] as TDefRange).Vars[i].Name = sel) then
        begin
          if (FDefRanges[n] as TDefRange).Vars[i].Line > l then
            Continue;
          AllowMouseLink := True;
          Exit;
        end;
  AllowMouseLink := False;
end;

function TEditorFrame.GetAtPoint(p: TPoint): string;
var
  s: integer;
  i: integer;
  len: integer;
  slen: integer;
  ln: string;
begin
  Result := '';
  ln := CodeEditor.Lines[p.y - 1];
  if ln = '' then
    Exit;
  slen := Length(ln);
  i := p.x - 1;
  len := 0;

  if i < 1 then
    i := 1;
  if (i < slen) and (ln[i + 1] in ['_', '0'..'9', 'a'..'z', 'A'..'Z', '$', '@', '#']) and
    ((i > 0) or (ln[i] in [#0..#32])) then
    Inc(i);

  while (i > 0) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
    Dec(i);

  if (i > 0) and (ln[i] in ['$', '@', '#']) then
  begin
    Inc(len);
    s := i;
    Inc(i);
  end
  else
  begin
    Inc(i);
    s := i;
  end;

  while (i <= slen) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
  begin
    Inc(i);
    Inc(len);
  end;
  Result := Copy(ln, s, len);

end;

procedure TEditorFrame.CodeJump(p: TPoint);
var
  i: Integer;
begin
  if (p.x = 0) and (p.y<=CodeEditor.Lines.Count) then
    for i:=1 to Length(CodeEditor.Lines[p.y-1]) do
      if CodeEditor.Lines[p.y-1][i] > #32 then
      begin
        p.x := i;
        break;
      end;
  CodeEditor.LogicalCaretXY := p;
  CodeEditor.TopLine := p.y;
  try
  CodeEditor.SetFocus;
  except
    on E: EInvalidOperation do ;
  end;
end;

procedure TEditorFrame.CodeEditorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);

  function GetCurrFunc(sel: string; out f: TFuncInfo): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FFunctions.Count - 1 do
      if LowerCase(Copy(FFunctions[i].Name, 1, Pos('(', FFunctions[i].Name) - 1)) =
        LowerCase(sel) then
      begin
        Result := True;
        f := FFunctions[i];
        if f.FileName = '' then
          exit;
      end;
  end;

  function GetCurrVar(sel: string; out v: TVarInfo): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FVars.Count - 1 do
    begin
      if isEnd(FVars[i].Name, sel) then
      begin
        Result := True;
        v := FVars[i];
        if v.FileName = '' then
          exit;
      end;
    end;
  end;

  function GetCurrInclude(out s: string): boolean;
  var
    ln: string;
  begin
    s := '';
    ln := CodeEditor.Lines[CodeEditor.PixelsToLogicalPos(Point(x, y)).y - 1];
    if isEnd(ln, '#include') then
    begin
      if pos('<', ln) > 0 then
        s := Trim(ExtractBetween(ln, '<', '>'))
      else if pos('"', ln) > 0 then
        s := Trim(ExtractBetween(ln, '"', '"'));
    end;
    Result := Length(s) > 0;
  end;

var
  sel: string;
  i, n, l: integer;
  v: TVarInfo;
  f: TFuncInfo;
  p: TPoint;
begin
  CodeEditor.Invalidate;
  if (Button = mbLeft) and (ssCtrl in Shift) then
  begin
    CodeEditor.LogicalCaretXY := CodeEditor.PixelsToLogicalPos(Point(x, y));
    sel := GetCurrWord;
    if GetCurrFunc(sel, f) then
    begin
      if (f.FileName = '') then
        CodeJump(Point(Pos(f.Name, CodeEditor.Lines[f.Line]), f.Line + 1))
      else if Assigned(FOpenEditor) then
        FOpenEditor(f.FileName, Point(1, f.Line + 1));
    end
    else if GetCurrVar(sel, v) then
    begin
      if (v.FileName = '') then
      begin
        if pos('[', v.Name) > 0 then
          CodeJump(Point(Pos(Copy(v.Name, 1, Pos('[', v.Name) - 1),
            CodeEditor.Lines[v.Line]), v.Line + 1))
        else
          CodeJump(Point(v.Pos, v.Line + 1));
      end
      else if Assigned(FOpenEditor) then
      begin
        FOpenEditor(v.FileName, Point(v.Pos + 1, v.Line + 1));
      end;
    end
    else if GetCurrInclude(sel) then
    begin
      sel := GetFullPath(sel, FIncludePath, ExtractFilePath(FFileName), FProject.Paths);
      if (sel <> '') and Assigned(OpenEditor) then
        OpenEditor(sel, Point(0, 0));
    end
    else
    begin
      l := CodeEditor.LogicalCaretXY.y - 1;
      for n := 0 to FDefRanges.Count - 1 do
        for i := 0 to (FDefRanges[n] as TDefRange).Vars.Count - 1 do
          if (l >= (FDefRanges[n] as TDefRange).StartLine) and
            (l <= (FDefRanges[n] as TDefRange).EndLine) and
            ((FDefRanges[n] as TDefRange).Vars[i].Name = sel) then
          begin
            if (FDefRanges[n] as TDefRange).Vars[i].Line > l then
              Continue;
            CodeJump(Point((FDefRanges[n] as TDefRange).Vars[i].Pos,
              (FDefRanges[n] as TDefRange).Vars[i].Line + 1));
          end;
    end;
  end;
  if (Button = mbLeft) then
  begin
    p := GetTemplatePos(True);
    if p.x > 0 then
    begin
      CodeEditor.BlockBegin := Point(p.x, CodeEditor.LogicalCaretXY.y);
      CodeEditor.BlockEnd := Point(p.y, CodeEditor.LogicalCaretXY.y);
      CodeEditor.LogicalCaretXY := Point(p.y, CodeEditor.LogicalCaretXY.y);
    end;
  end;
end;

procedure TEditorFrame.CodeExplorerCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: boolean);
begin
  Sender.Canvas.Font.Color := Sender.Color;
end;

procedure TEditorFrame.CodeExplorerDblClick(Sender: TObject);
begin
  if not Assigned(CodeExplorer.Selected) then
    Exit;
  case CodeExplorer.Selected.ImageIndex of
    1: if Assigned(FOpenEditor) then
        OpenEditor(GetFullPath(FRequiredFiles[IntPtr(CodeExplorer.Selected.Data)],
          IncludePath, ExtractFilePath(FFileName), FProject.Paths), Point(0, 0));
    2:
      if CodeExplorer.Selected.Parent.ImageIndex = 0 then
        CodeJump(Point(1, FFunctions[IntPtr(CodeExplorer.Selected.Data)].Line + 1))
      else if Assigned(FOpenEditor) then
        OpenEditor(GetFullPath(
          FRequiredFiles[IntPtr(CodeExplorer.Selected.Parent.Data)],
          FIncludePath, ExtractFilePath(FFileName), FProject.Paths),
          Point(1, FFunctions[IntPtr(CodeExplorer.Selected.Data)].Line + 1));
    3:
      if CodeExplorer.Selected.Parent.ImageIndex = 0 then
        CodeJump(Point(FVars[IntPtr(CodeExplorer.Selected.Data)].Pos,
          FVars[IntPtr(CodeExplorer.Selected.Data)].Line + 1))
      else if Assigned(FOpenEditor) then
        OpenEditor(GetFullPath(
          FRequiredFiles[IntPtr(CodeExplorer.Selected.Parent.Data)],
          FIncludePath, ExtractFilePath(FFileName), FProject.Paths),
          Point(FVars[IntPtr(CodeExplorer.Selected.Data)].Pos,
          FVars[IntPtr(CodeExplorer.Selected.Data)].Line + 1));
  end;
end;

procedure TEditorFrame.CodeEditorChange(Sender: TObject);
begin
  UpdateTimerTimer(nil);
  if Assigned(FOnChange) then
    FOnChange(Self);
  CodeEditor.Invalidate;
end;

procedure TEditorFrame.CodeEditorKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
function GetTab: String; inline;
begin
  SetLength(Result, CodeEditor.TabWidth);
  FillChar(Result[1], CodeEditor.TabWidth, ' ');
end;

var
  p: TPoint;
  i: Integer;
begin
  FillChar(p, SizeOf(p), 0);
  if Key = 9 then
  begin
    if CodeEditor.BlockBegin.y<CodeEditor.BlockEnd.y then
    begin
      for i:=CodeEditor.BlockBegin.y-1 to CodeEditor.BlockEnd.y-1 do
        CodeEditor.Lines[i] := GetTab + CodeEditor.Lines[i];
      key:=0;
      exit;
    end;
    p := GetTemplatePos(False);
    if p.x > 0 then
      Key := 0;
  end
  //else if Key = 13 then // Dont know what this was in first place
    //CodeEditor.TextBetweenPoints[Point(0, CodeEditor.LogicalCaretXY.Y),
      //Point(Length(CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 1]) + 1,
      //CodeEditor.LogicalCaretXY.Y)] := TrimRight(CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 1])
  else if Key = VK_BACK then
    p := GetTemplatePos(True);
  if p.x > 0 then
  begin
    CodeEditor.BlockBegin := Point(p.x, CodeEditor.LogicalCaretXY.Y);
    CodeEditor.BlockEnd := Point(p.y, CodeEditor.LogicalCaretXY.Y);
    CodeEditor.LogicalCaretXY := Point(p.y, CodeEditor.LogicalCaretXY.y);
  end
  else if (Key = VK_SPACE) and (ssCtrl in Shift) then
    if CodeEditor.TextBetweenPoints[CodeEditor.BlockBegin,
      CodeEditor.BlockEnd] <> '' then
    begin
      p := CodeEditor.BlockBegin;
      CodeEditor.TextBetweenPoints[CodeEditor.BlockBegin, CodeEditor.BlockEnd] := '';
      CodeEditor.LogicalCaretXY := p;
    end;
end;


procedure TEditorFrame.StartFormatter;
var
  c: TCodeFormatter;
  i: integer;
begin
  if MessageDlg(SCodeFormatterTitle, SCodeFormatterText,
    mtConfirmation, mbYesNo, SCodeFormatterKeyword) = mrYes then
  begin
    c := TCodeFormatter.Create;
    try
      c.Lines.Clear;
      c.Lines.AddStrings(CodeEditor.Lines);
      c.Format;
      for i := 0 to CodeEditor.Lines.Count - 1 do
        if CodeEditor.Lines[i] <> c.Lines[i] then
        begin
          CodeEditor.TextBetweenPoints[Point(0, i + 1),
            Point(Length(CodeEditor.Lines[i]) + 1, i + 1)] := c.Lines[i];
        end;
    finally
      c.Free;
    end;

  end;
end;

procedure TEditorFrame.CheckSelTimerTimer(Sender: TObject);

  function isParam(out n: integer; out s: string): boolean;
  var
    i, len: integer;
    ln: string;
    d: integer;
    instr: boolean;
  begin
    Result := False;
    instr := False;
    repeat
      n := 0;
      d := 1;
      i := CodeEditor.LogicalCaretXY.x - 1;
      ln := CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 1];
      instr := not instr;
      while (i > 0) and (i <= Length(ln)) and (d > 0) do
      begin
        if ln[i] = '"' then
          instr := not instr
        else if not instr then
          if ln[i] = ')' then
            Inc(d)
          else if ln[i] = '(' then
            Dec(d);
        if (d = 1) and (ln[i] = ',') and not instr then
          Inc(n);
        Dec(i);
      end;
    until not instr;
    if d = 0 then
    begin
      len := 0;
      while (i > 0) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z', '#']) do
      begin
        Dec(i);
        Inc(len);
      end;
      if len > 0 then
        s := Copy(ln, i + 1, len);
      Result := True;
    end;
  end;

  function isStdFunc(s: string; out x: integer): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FStdFunc.Count - 1 do
    begin
      if AnsiStartsText(s + '(', FStdFunc[i].Name) then
      begin
        Result := True;
        x := i;
        Exit;
      end;
    end;
  end;

  function isInFunc(s: string; out x: integer): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FFunctions.Count - 1 do
    begin
      if AnsiStartsText(s + '(', FFunctions[i].Name) then
      begin
        Result := True;
        x := i;
        Exit;
      end;
    end;
  end;

var
  s, m, inf: string;
  i, n: integer;
  tmp: string;
begin
  if (CodeEditor.TopLine <> Topl) then
  begin
    if FToolTip.Visible then
    begin
      FToolTip.Hide;
      CodeEditor.Invalidate;
      currFunc := '';
    end;
    topl := CodeEditor.TopLine;
  end;
  tmp := lowercase(GetCurrWord());
  if tmp <> currWord then
  begin
    currWord := tmp;
    Highlight.SelectedText := '';
    CodeEditor.Invalidate;
    //Reset
    SelectHighlightTimer.Enabled := False;
    SelectHighlightTimer.Enabled := True;
  end;
  m := '';
  inf := '';
  if isParam(n, s) then
  begin
    if isStdFunc(s, i) then
    begin
      m := FStdFunc[i].Name;
      inf := FStdFunc[i].Info;
    end
    else if isInFunc(s, i) then
    begin
      m := FFunctions[i].Name;
      inf := FFunctions[i].Info;
    end;
  end
  else
  begin
    n := -1;
    if isStdFunc(tmp, i) then
    begin
      m := FStdFunc[i].Name;
      inf := FStdFunc[i].Info;
    end
    else if isInFunc(tmp, i) then
    begin
      m := FFunctions[i].Name;
      inf := FFunctions[i].Info;
    end;
  end;
  if isEnd(CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 1], 'func') then
    m := '';
  FToolTip.SelectedParam := n;
  if Length(m) > 0 then
  begin
    if LowerCase(m) <> LowerCase(currFunc) then
    begin
      currInfo := inf;
      currFunc := m;
      FToolTip.Hide;
      ToolTipTimer.Enabled := False;
      ToolTipTimer.Enabled := True;
    end;
  end
  else
  begin
    currFunc := '';
    FToolTip.Hide;
  end;
end;

procedure TEditorFrame.CloseCodeExplorerButtonClick(Sender: TObject);
begin
  if CloseCodeExplorerButton.Caption = 'x' then
  begin
    CodeExplorer.Hide;
    CodeExplorerPanel.Width := 32;
    CloseCodeExplorerButton.Caption := '<';
    CodeExplorerHead.Caption := '';
  end
  else
  begin
    CodeExplorer.Show;
    CodeExplorerPanel.Width := 235;
    CloseCodeExplorerButton.Caption := 'x';
    CodeExplorerHead.Caption := SCodeExplorer;
    UpdateTimerTimer(nil);
  end;
end;

procedure TEditorFrame.CloseSearchButtonClick(Sender: TObject);
begin
  SearchBar.Hide;
  CodeEditor.SetFocus;
end;

function TEditorFrame.GetCurrWord(): string;
var
  s: integer;
  i: integer;
  len: integer;
  slen: integer;
  ln: string;
begin
  Result := '';
  ln := CodeEditor.Lines[CodeEditor.LogicalCaretXY.y - 1];
  if ln = '' then
    Exit;
  slen := Length(ln);
  i := CodeEditor.LogicalCaretXY.x - 1;
  len := 0;

  if i < 1 then
    i := 1;
  if (i < slen) and (ln[i + 1] in ['_', '0'..'9', 'a'..'z', 'A'..'Z', '$', '#', '@']) and
    ((i > 0) or (ln[i] in [#0..#32])) then
    Inc(i);

  while (i > 0) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
    Dec(i);

  if (i > 0) and (ln[i] in ['$', '#', '@']) then
  begin
    Inc(len);
    s := i;
    Inc(i);
  end
  else
  begin
    Inc(i);
    s := i;
  end;

  while (i <= slen) and (ln[i] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
  begin
    Inc(i);
    Inc(len);
  end;
  Result := Copy(ln, s, len);

end;

procedure TEditorFrame.SetFunc(l: TFuncList);
begin
  FFunctions.Clear;
  FFunctions.Assign(l);
end;

procedure TEditorFrame.SetVar(l: TVarList);
begin
  FVars.Clear;
  FVars.Assign(l);
end;

destructor TEditorFrame.Destroy;
var
  i: integer;
begin
  FToolTip.Free;
  if not (Parser.Finished) then
    Parser.Terminate;
  Parser.Free;
  for i := 0 to FDefRanges.Count - 1 do
    FDefRanges.Items[i].Free;
  FRequiredFiles.Free;
  FDefRanges.Free;
  FFunctions.Free;
  FVars.Free;
  FStdFunc.Free;
  FKeyWords.Free;
  CodeEditor.Highlighter := nil;
  Highlight.Free;
  FIncludeFiles.Free;
  FCompVars.Free;
  FMacros.Free;
  FUseCount.Free;
  FCompletionList.Free;
  inherited;
end;

end.
