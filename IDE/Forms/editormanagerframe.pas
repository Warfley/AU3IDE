unit EditorManagerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, ComCtrls, Editor, FormEditor,
  Dialogs, Buttons, Menus, ExtCtrls, au3Types, Project, fgl, EditorWindow;

type
  TCloseEditorEvent = procedure(Sender: TObject; Editor: integer;
    var Proceed: boolean) of object;
  TEditorNotifyEvent = procedure(Sender: TObject; Editor: integer) of object;

  TEditorList = specialize TFPGList<TTabSheet>;
  TViewWindowList = specialize TFPGList<TEditorViewForm>;

  { TEditorManager }

  TEditorManager = class(TFrame)
    EditorControl: TPageControl;
    EditorControl1: TPageControl;
    EditorControl2: TPageControl;
    EditorControl3: TPageControl;
    FilesPopUp: TPopupMenu;
    BLMoveMenu: TMenuItem;
    BRMoveMenu: TMenuItem;
    MenuItem1: TMenuItem;
    MoveToButton2: TSpeedButton;
    MoveToButton3: TSpeedButton;
    MoveToButton4: TSpeedButton;
    NewWindowMoveMenu: TMenuItem;
    OpenAU3FileDialog: TOpenDialog;
    OpenEditorButton2: TSpeedButton;
    OpenEditorButton3: TSpeedButton;
    OpenEditorButton4: TSpeedButton;
    TRMoveMenu: TMenuItem;
    TLMoveMenu: TMenuItem;
    MoveToPopup: TPopupMenu;
    LPanelTop: TPanel;
    LPanelBot: TPanel;
    LRSplitterBot: TSplitter;
    RPanelTop: TPanel;
    LRSplitterTop: TSplitter;
    RPanelBot: TPanel;
    OpenEditorButton1: TSpeedButton;
    MoveToButton1: TSpeedButton;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    TopBotSplit: TSplitter;
    TopPanel: TPanel;
    SplitBotButton: TSpeedButton;
    SplitTopButton: TSpeedButton;
    SplitVButton: TSpeedButton;
    BotPanel: TPanel;
    ViewBar: TToolBar;
    procedure EditorControlChange(Sender: TObject);
    procedure EditorControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MoveToButton1Click(Sender: TObject);
    procedure NewWindowMoveMenuClick(Sender: TObject);
    procedure OpenEditorButton1Click(Sender: TObject);
    procedure SplitBotButtonClick(Sender: TObject);
    procedure SplitTopButtonClick(Sender: TObject);
    procedure SplitVButtonClick(Sender: TObject);
    procedure TLMoveMenuClick(Sender: TObject);
  private
    { Fields }
    FFocused: integer;
    FProject: Tau3Project;
    FIncludePath: string;
    FOnEditorClose: TCloseEditorEvent;
    FOnEditorCreated: TEditorNotifyEvent;
    FOnEditorChanged: TNotifyEvent;
    FOpenEditor: TOpenEditorEvent;
    FEnterFunc: TOpenFunctionEvent;
    FOnParserFinished: TNotifyEvent;
    FTabs: TEditorList;
    FViewWindows: TViewWindowList;
    { Functions & Procedures }
    function GetFocused(View: integer): integer;
    function GetPageControl(View: integer): TPageControl;
    function GetView(i: integer): integer;
    function GetViewOpened(View: integer): boolean;
    function GetWindowCount: integer;
    procedure SetFocused(View: integer; AValue: integer);
    procedure SetView(EIndex: integer; AValue: integer);
    procedure ProjectItemClick(Sender: TObject);
    procedure CurrentItemClick(Sender: TObject);
    procedure OpenNewItemClick(Sender: TObject);
    procedure MoveTab(TabIndex: integer; Dest: TPageControl);
    procedure EditorEnter(Sender: TObject);
    procedure SetIncludePath(s: string);
    function FindTextEditor(FileName: string): TEditorFrame;
    function FindFormEditor(FileName: string): TFormEditFrame;
    function FindEditor(FileName: string): integer;
    function GetEditor(i: integer): TFrame;
    function GetCurrentEditor: TFrame;
    procedure SetCurrentEditor(f: TFrame);
    function GetIndex: integer;
    procedure SetIndex(i: integer);
    procedure CreateEditor(FName: string; Line, Pos: integer; View: TPageControl);
    procedure EditorChanged(Sender: TObject);
    function GetFileName(i: integer): string;
    procedure SetFileName(i: integer; s: string);
    function GetCount: integer;
    function GetEditorCaret(i: integer): TPoint;
    procedure SetEditorCaret(i: integer; p: TPoint);
    function FindIndex(T: TTabSheet): integer;
    procedure SetViewOpened(View: integer; AValue: boolean);
    procedure SetWindowCount(AValue: integer);
    function CreateViewWindow: TEditorViewForm;
    procedure ViewWindowClose(Sender: TObject; var act: TCloseAction);
  public
    function OpenEditor(FileName: string; Pos: TPoint; View: integer = 0): TFrame;
    procedure CloseEditor(i: integer);
    procedure EditorSave(i: integer; p: string = ''); overload;
    procedure EditorSave(Editor: TFrame; p: string = ''); overload;
    procedure EditorLoad(i: integer; p: string = ''); overload;
    procedure EditorLoad(Editor: TFrame; p: string = ''); overload;
    { Properties }
    property TextEditor[FileName: string]: TEditorFrame read FindTextEditor;
    property FormEditor[FileName: string]: TFormEditFrame read FindFormEditor;
    property Editor[FileName: string]: integer read FindEditor;
    property Editors[i: integer]: TFrame read GetEditor;
    property CurrentEditor: TFrame read GetCurrentEditor write SetCurrentEditor;
    property EditorIndex: integer read GetIndex write SetIndex;
    property EditorFiles[i: integer]: string read GetFileName write SetFileName;
    property Count: integer read GetCount;
    property EditorCaret[i: integer]: TPoint read GetEditorCaret write SetEditorCaret;
    property Project: Tau3Project read FProject write FProject;
    property IncludePath: string read FIncludePath write SetIncludePath;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property EditorView[EIndex: integer]: integer read GetView write SetView;
    property ViewOpened[View: integer]: boolean read GetViewOpened write SetViewOpened;
    property WindowCount: integer read GetWindowCount write SetWindowCount;
    property Focused[View: integer]: integer read GetFocused write SetFocused;
    { Events }
    property OnEditorClose: TCloseEditorEvent read FOnEditorClose write FOnEditorClose;
    property OnEditorCreated: TEditorNotifyEvent
      read FOnEditorCreated write FOnEditorCreated;
    property OnEditorChanged: TNotifyEvent read FOnEditorChanged write FOnEditorChanged;
    property IDEOpenFile: TOpenEditorEvent read FOpenEditor write FOpenEditor;
    property EnterFunc: TOpenFunctionEvent read FEnterFunc write FEnterFunc;
    property OnParserFinished: TNotifyEvent read FOnParserFinished
      write FOnParserFinished;
  end;

implementation

{$R *.lfm}

{ TEditorManager }

procedure TEditorManager.EditorControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  EC: TPageControl;
begin
  EC := Sender as TPageControl;
  if Button = mbMiddle then
    CloseEditor(FindIndex(ec.Pages[EC.TabIndexAtClientPos(Point(X, Y))]));
end;

procedure TEditorManager.MoveToButton1Click(Sender: TObject);
var
  p: TPoint;
  c: TControl;
  e: TPageControl;
begin
  c := Sender as TControl;
  NewWindowMoveMenu.Tag := c.Tag;
  MoveToPopup.Tag := c.Tag;
  TRMoveMenu.Visible := SplitTopButton.Down;
  BLMoveMenu.Visible := SplitVButton.Down;
  BRMoveMenu.Visible := SplitVButton.Down and SplitBotButton.Down;
  p := c.Parent.ClientToScreen(Point(c.Left, c.Top + c.Height));
  e := GetPageControl(c.Tag);
  if Assigned(e.ActivePage) then
    MoveToPopup.PopUp(p.x, p.y);
end;

procedure TEditorManager.NewWindowMoveMenuClick(Sender: TObject);
var
  p: TPageControl;
  w: TEditorViewForm;
begin
  p := GetPageControl(MoveToPopup.Tag);
  w:=CreateViewWindow;
  MoveTab(FindIndex(p.ActivePage), w.EditorControl);
end;

procedure TEditorManager.OpenEditorButton1Click(Sender: TObject);
var
  i: integer;
  ProjItem, tmpItem: TMenuItem;
  p: TPoint;
  c: TControl;
begin
  while FilesPopUp.Items.Count > 0 do
    FilesPopUp.Items[0].Free;
  ProjItem := TMenuItem.Create(FilesPopUp);
  FilesPopUp.Items.Add(ProjItem);
  ProjItem.Caption := 'Projekt';
  tmpItem := TMenuItem.Create(ProjItem);
  tmpItem.Caption := ExtractFileName(FProject.MainFile);
  tmpItem.Tag := -1;
  tmpItem.OnClick := @ProjectItemClick;
  ProjItem.Add(tmpItem);
  for i := 0 to FProject.Files.Count - 1 do
  begin
    tmpItem := TMenuItem.Create(ProjItem);
    tmpItem.Caption := FProject.Files[i];
    tmpItem.Tag := i;
    tmpItem.OnClick := @ProjectItemClick;
    ProjItem.Add(tmpItem);
  end;
  ProjItem := TMenuItem.Create(FilesPopUp);
  FilesPopUp.Items.Add(ProjItem);
  ProjItem.Caption := 'Offen';
  for i := 0 to Count - 1 do
  begin
    tmpItem := TMenuItem.Create(ProjItem);
    tmpItem.Caption := ExtractFileName(EditorFiles[i]);
    tmpItem.Tag := i;
    tmpItem.OnClick := @CurrentItemClick;
    ProjItem.Add(tmpItem);
  end;
  ProjItem := TMenuItem.Create(FilesPopUp);
  FilesPopUp.Items.Add(ProjItem);
  ProjItem.Caption := '-';

  ProjItem := TMenuItem.Create(FilesPopUp);
  FilesPopUp.Items.Add(ProjItem);
  ProjItem.Caption := 'Datei...';
  ProjItem.OnClick := @OpenNewItemClick;

  c := Sender as TControl;
  p := c.Parent.ClientToScreen(Point(c.Left, c.Top + c.Height));
  FilesPopUp.Tag := c.Tag;
  FilesPopUp.PopUp(p.x, p.y);
end;

procedure TEditorManager.SplitBotButtonClick(Sender: TObject);
begin
  if not SplitBotButton.Down then
    while EditorControl3.PageCount > 0 do
      MoveTab(FindIndex(EditorControl3.ActivePage), EditorControl2);
  RPanelBot.Width := Self.Width div 2;
  RPanelBot.Visible := SplitBotButton.Down;
  LRSplitterBot.Visible := SplitBotButton.Down;
  if Assigned(FOnEditorChanged) then
    FOnEditorChanged(Self);
end;

procedure TEditorManager.SplitTopButtonClick(Sender: TObject);
begin
  if not SplitTopButton.Down then
    while EditorControl1.PageCount > 0 do
      MoveTab(FindIndex(EditorControl1.ActivePage), EditorControl);
  RPanelTop.Width := Self.Width div 2;
  RPanelTop.Visible := SplitTopButton.Down;
  LRSplitterTop.Visible := SplitTopButton.Down;
  if Assigned(FOnEditorChanged) then
    FOnEditorChanged(Self);
end;

procedure TEditorManager.SplitVButtonClick(Sender: TObject);
begin
  if not SplitVButton.Down then
  begin
    while EditorControl2.PageCount > 0 do
      MoveTab(FindIndex(EditorControl2.ActivePage), EditorControl);
    while EditorControl3.PageCount > 0 do
      MoveTab(FindIndex(EditorControl3.ActivePage), EditorControl);
  end;
  BotPanel.Height := Self.Height div 2;
  BotPanel.Visible := SplitVButton.Down;
  SplitBotButton.Visible := SplitVButton.Down;
  TopBotSplit.Visible := SplitVButton.Down;
  SplitBotButton.Left := 0;
  SplitVButton.Left := 0;
  SplitTopButton.Left := 0;
  if Assigned(FOnEditorChanged) then
    FOnEditorChanged(Self);
end;

procedure TEditorManager.TLMoveMenuClick(Sender: TObject);
var
  tmp: TTabSheet;
begin
  tmp := GetPageControl(MoveToPopup.Tag).ActivePage;
  SetView(FindIndex(tmp), (Sender as TMenuItem).Tag);
end;

function TEditorManager.GetPageControl(View: integer): TPageControl;
begin
  case View of
    0: Result := EditorControl;
    1: Result := EditorControl1;
    2: Result := EditorControl2;
    3: Result := EditorControl3;
    else Result:=FViewWindows[View-4].EditorControl;
  end;
end;

function TEditorManager.GetFocused(View: integer): integer;
begin
  Result := FindIndex(GetPageControl(View).ActivePage);
end;

function TEditorManager.GetView(i: integer): integer;
begin
  Result := FTabs[i].PageControl.Tag;
end;

function TEditorManager.GetViewOpened(View: integer): boolean;
begin
  case View of
    0: Result:=True;
    1: Result:=SplitTopButton.Down;
    2: Result:=SplitVButton.Down;
    3: Result:=SplitVButton.Down and SplitBotButton.Down;
    else Result:=(View>3) and (View<WindowCount-4);
  end;
end;

function TEditorManager.GetWindowCount: integer;
begin
  Result := FViewWindows.Count;
end;

procedure TEditorManager.SetFocused(View: integer; AValue: integer);
var
  i: integer;
  p: TPageControl;
begin
  if not ViewOpened[View] then exit;
  p := GetPageControl(View);
  if FTabs[FFocused].PageControl = p then
    SetIndex(AValue)
  else
    for i := 0 to p.PageCount-1 do
      if p.Page[i] = FTabs[AValue] then
        p.PageIndex := i;
end;

procedure TEditorManager.SetView(EIndex: integer; AValue: integer);
begin
  MoveTab(EIndex, GetPageControl(AValue));
end;

procedure TEditorManager.ProjectItemClick(Sender: TObject);
var
  i: integer;
  f: string;
begin
  if (Sender as TMenuItem).Tag >= 0 then
    f := FProject.FilePath[(Sender as TMenuItem).Tag]
  else
    f := FProject.MainFile;
  i := Editor[f];
  if i < 0 then
    CreateEditor(f, 0, 0, GetPageControl(FilesPopUp.Tag))
  else
  begin
    SetView(i, FilesPopUp.Tag);
  end;
end;

procedure TEditorManager.CurrentItemClick(Sender: TObject);
begin
  SetView((Sender as TMenuItem).Tag, FilesPopUp.Tag);
end;

procedure TEditorManager.OpenNewItemClick(Sender: TObject);
var
  i: integer;
begin
  if OpenAU3FileDialog.Execute then
  begin
    i := Editor[OpenAU3FileDialog.FileName];
    if i < 0 then
      CreateEditor(OpenAU3FileDialog.FileName, 0, 0, GetPageControl(FilesPopUp.Tag))
    else
      SetView(i, FilesPopUp.Tag);
  end;
end;

procedure TEditorManager.MoveTab(TabIndex: integer; Dest: TPageControl);
var
  tmp: TTabSheet;
  e: TFrame;
begin
  if FTabs[TabIndex].PageControl <> Dest then
  begin
    tmp := TTabSheet.Create(Dest);
    tmp.Caption := FTabs[TabIndex].Caption;
    tmp.Visible := True;
    tmp.PageControl := Dest;
    e := Editors[TabIndex];
    e.Parent := tmp;
    e.Align := alClient;
    FTabs[TabIndex].Free;
    FTabs[TabIndex] := tmp;
    Dest.ActivePage := tmp;
  end;
  if Assigned(FOnEditorChanged) then
    FOnEditorChanged(Self);
end;

procedure TEditorManager.EditorEnter(Sender: TObject);
begin
  FFocused := FindIndex(Sender as TTabSheet);
end;

procedure TEditorManager.EditorControlChange(Sender: TObject);
begin
  FFocused := FindIndex((Sender as TPageControl).ActivePage);
  if Assigned(FOnEditorChanged) then
    FOnEditorChanged(Self);
end;

procedure TEditorManager.SetIncludePath(s: string);
var
  i: integer;
begin
  FIncludePath := s;
  for i := 0 to Count - 1 do
    if Editors[i] is TEditorFrame then
      (Editors[i] as TEditorFrame).IncludePath := s;
end;

function TEditorManager.FindTextEditor(FileName: string): TEditorFrame;
var
  i: integer;
begin
  i := FindEditor(FileName);
  if (i >= 0) and (Editors[i] is TEditorFrame) then
    Result := Editors[i] as TEditorFrame
  else
    Result := nil;
end;

function TEditorManager.FindFormEditor(FileName: string): TFormEditFrame;
var
  i: integer;
begin
  i := FindEditor(FileName);
  if (i >= 0) and (Editors[i] is TFormEditFrame) then
    Result := Editors[i] as TFormEditFrame
  else
    Result := nil;
end;

function TEditorManager.FindEditor(FileName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FTabs.Count - 1 do
    if ((FTabs[i].Controls[0] is TEditorFrame) and
      ((FTabs[i].Controls[0] as TEditorFrame).FileName = FileName)) or
      ((FTabs[i].Controls[0] is TFormEditFrame) and
      ((FTabs[i].Controls[0] as TFormEditFrame).FileName = FileName)) then
    begin
      Result := i;
      Exit;
    end;
end;

function TEditorManager.GetEditor(i: integer): TFrame;
begin
  if (i >= 0) and (i < Count) then
    Result := FTabs[i].Controls[0] as TFrame
  else
    Result := nil;
end;

function TEditorManager.GetCurrentEditor: TFrame;
begin
  if (FFocused >= 0) and (FFocused < FTabs.Count) then
    Result := Editors[FFocused]
  else
    Result := nil;
end;

procedure TEditorManager.SetCurrentEditor(f: TFrame);
var
  i: integer;
begin
  for i := 0 to FTabs.Count - 1 do
    if FTabs[i].Controls[0] = f then
    begin
      if Assigned(FTabs[i].PageControl) then
        FTabs[i].PageControl.ActivePage := FTabs[i];
      FTabs[i].SetFocus;
      FFocused:=i;
      Break;
    end;
end;

function TEditorManager.GetIndex: integer;
begin
  Result := FFocused;
  if Result >= Count then
    Result := Count - 1;
end;

procedure TEditorManager.SetIndex(i: integer);
begin
  if (i >= 0) and (i < Count) then
    SetCurrentEditor(Editors[i]);
  FFocused := i;
end;

procedure TEditorManager.EditorChanged(Sender: TObject);
begin
  if not Assigned((Sender as TControl).Parent) then
    exit;
  if not ((Sender as TFrame).Parent.Caption[1] = '*') then
    (Sender as TFrame).Parent.Caption := '*' + (Sender as TFrame).Parent.Caption;
  if Assigned(FOnEditorChanged) then
    FOnEditorChanged(Self);
end;

function TEditorManager.GetFileName(i: integer): string;
begin
  if Editors[i] is TEditorFrame then
    Result := (Editors[i] as TEditorFrame).FileName
  else if Editors[i] is TFormEditFrame then
    Result := (Editors[i] as TFormEditFrame).FileName;
end;

procedure TEditorManager.SetFileName(i: integer; s: string);
begin
  if Editors[i] is TEditorFrame then
    (Editors[i] as TEditorFrame).FileName := s
  else if Editors[i] is TFormEditFrame then
    (Editors[i] as TFormEditFrame).FileName := s;
end;

function TEditorManager.GetCount: integer;
begin
  Result := FTabs.Count;
end;

function TEditorManager.GetEditorCaret(i: integer): TPoint;
begin
  FillChar(Result, SizeOf(Result), #00);
  if Editors[i] is TEditorFrame then
    Result := (Editors[i] as TEditorFrame).CodeEditor.LogicalCaretXY;
end;

procedure TEditorManager.SetEditorCaret(i: integer; p: TPoint);
begin
  if Editors[i] is TEditorFrame then
    (Editors[i] as TEditorFrame).CodeEditor.LogicalCaretXY := p;
end;

function TEditorManager.FindIndex(T: TTabSheet): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FTabs.Count - 1 do
    if FTabs[i] = T then
    begin
      Result := i;
      Break;
    end;
end;

procedure TEditorManager.SetViewOpened(View: integer; AValue: boolean);
begin
  case View of
    1:
    begin
      SplitTopButton.Down := AValue;
      SplitTopButtonClick(SplitBotButton);
    end;
    2:
    begin
      SplitVButton.Down := AValue;
      SplitVButtonClick(SplitVButton);
    end;
    3:
    begin
      if AValue then
        if not SplitVButton.Down then
          SetViewOpened(2, AValue);
      SplitBotButton.Down := AValue;
      SplitBotButtonClick(SplitBotButton);
    end;
    else if not AValue then FViewWindows[View-4].Close;
  end;
end;

procedure TEditorManager.SetWindowCount(AValue: integer);
var i: Integer;
begin
  i:=FViewWindows.Count+1;
  while (FViewWindows.Count>AValue) and (i>FViewWindows.Count) do
  begin
    i:=FViewWindows.Count;
    FViewWindows[FViewWindows.Count-1].Close;
  end;
  while FViewWindows.Count<AValue do
    CreateViewWindow;
end;

function TEditorManager.CreateViewWindow: TEditorViewForm;
begin
  Result:=TEditorViewForm.Create(self);
  Result.EditorControl.Tag:=FViewWindows.Add(Result)+4;
  Result.OpenEditorButton2.Tag:=Result.Tag;
  Result.EditorControl.OnChange:=@EditorControlChange;
  Result.EditorControl.OnMouseUp:=@EditorControlMouseUp;
  Result.OnClose:=@ViewWindowClose;
  Result.OpenEditorButton2.OnClick:=@OpenEditorButton1Click;
  Result.Show;
end;

procedure TEditorManager.ViewWindowClose(Sender: TObject; var act: TCloseAction);
var i: Integer;
 w: TEditorViewForm;
begin
  w:=Sender as TEditorViewForm;
  i:=w.EditorControl.PageCount+1;
  while (w.EditorControl.PageCount>0) and (i>w.EditorControl.PageCount) do
  begin
    i:=w.EditorControl.PageCount;
    CloseEditor(FindIndex(w.EditorControl.Pages[0]));
  end;
  if w.EditorControl.PageCount>0 then
    act:=caNone
  else
    act:=caFree;
  FViewWindows.Delete(FViewWindows.IndexOf(w));
  for i:=0 to FViewWindows.Count-1 do
  begin
    FViewWindows[i].EditorControl.Tag:=4+i;
    FViewWindows[i].OpenEditorButton2.Tag:=4+i;
  end;
end;

procedure TEditorManager.CreateEditor(FName: string; Line, Pos: integer;
  View: TPageControl);
var
  tmp: TTabSheet;
  ext: string;
begin
  tmp := View.AddTabSheet;
  tmp.Caption := ExtractFileName(FName);
  tmp.Visible := True;
  FFocused := FTabs.Add(tmp);
  View.ActivePage := tmp;
  ext := ExtractFileExt(FName);
  if ext = '.afm' then
    with TFormEditFrame.Create(Self) do
    begin
      Align := alClient;
      Parent := tmp;
      Visible := True;
      Name := Format('FormEditor%d', [FFocused]);
      OnChange := @EditorChanged;
      OnVarChanged := FOnParserFinished;
      OpenEditor := FOpenEditor;
      EnterFunc := FEnterFunc;
      if FName = GetFullPath(FProject.MainForm, IncludePath,
        FProject.ProjectDir, FProject.Paths) then
        SetMainForm(True, True);
      if FileExists(FName) then
        Load(FName)
      else
        FileName := FName;
    end
  else if (ext = '.au3') or (ext = '.apr') then
    with TEditorFrame.Create(Self) do
    begin
      Align := alClient;
      Parent := tmp;
      Visible := True;
      Name := Format('CodeEditor%d', [FFocused]);
      CodeEditor.SetFocus;
      IncludePath := FIncludePath;
      OpenEditor := FOpenEditor;
      OnChange := @EditorChanged;
      Project := FProject;
      OnParserFinished := FOnParserFinished;
      if FileExists(FName) then
        Load(FName)
      else
        FileName := FName;
      if (Line > 0) and (Pos > 0) then
        CodeJump(Point(Pos, Line));
      CodeEditor.SetFocus;
    end;
  if Assigned(FOnEditorCreated) then
    OnEditorCreated(Self, EditorIndex);
end;

function TEditorManager.OpenEditor(FileName: string; Pos: TPoint;
  View: integer = 0): TFrame;
var
  Index: integer;
begin
  Index := FindEditor(FileName);
  if Index = -1 then
    CreateEditor(FileName, Pos.y, Pos.x, GetPageControl(View))
  else
  begin
    EditorView[Index] := View;
    SetCurrentEditor(Editors[Index]);
    if (GetCurrentEditor is TEditorFrame) then
    begin
      if (Pos.Y > 0) and (Pos.X > 0) then
        (GetCurrentEditor as TEditorFrame).CodeJump(Pos);
      GetCurrentEditor.SetFocus;
      (GetCurrentEditor as TEditorFrame).CodeEditor.SetFocus;
    end
    else if GetCurrentEditor is TFormEditFrame then
      (GetCurrentEditor as TFormEditFrame).EventEditor.Selection := Rect(0, 0, 0, 0);
  end;
  Result := GetCurrentEditor;
end;

procedure TEditorManager.CloseEditor(i: integer);
var
  Proceed: boolean;
begin
  Proceed := True;
  if Assigned(FOnEditorClose) then
    FOnEditorClose(Self, i, Proceed);
  if not Proceed or (i < 0) or (i >= Count) then
    Exit;
  if FFocused = i then
    if FTabs[i].PageControl.PageCount > 1 then
      FTabs[i].PageControl.SelectNextPage(FTabs[i].PageIndex = 0)
    else if i < Count - 1 then
      SetIndex(i + 1)
    else
      SetIndex(i - 1);
  FTabs[i].Controls[0].Free;
  FTabs[i].Free;
  FTabs.Delete(i);
end;

procedure TEditorManager.EditorSave(i: integer; p: string = '');
begin
  EditorSave(Editors[i], p);
end;

procedure TEditorManager.EditorSave(Editor: TFrame; p: string = '');
begin
  if Editor is TEditorFrame then
  begin
    (Editor as TEditorFrame).Save(p);
    Editor.Parent.Caption := ExtractFileName((Editor as TEditorFrame).FileName);
  end
  else if Editor is TFormEditFrame then
  begin
    (Editor as TFormEditFrame).Save(p);
    Editor.Parent.Caption := ExtractFileName((Editor as TFormEditFrame).FileName);
  end;
end;

procedure TEditorManager.EditorLoad(i: integer; p: string = '');
begin
  EditorLoad(Editors[i], p);
end;

procedure TEditorManager.EditorLoad(Editor: TFrame; p: string = '');
begin
  if Editor is TEditorFrame then
  begin
    (Editor as TEditorFrame).Load(p);
    Editor.Parent.Caption := ExtractFileName((Editor as TEditorFrame).FileName);
  end
  else if Editor is TFormEditFrame then
  begin
    (Editor as TFormEditFrame).Load(p);
    Editor.Parent.Caption := ExtractFileName((Editor as TFormEditFrame).FileName);
  end;
end;

constructor TEditorManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTabs := TEditorList.Create;
  FViewWindows:=TViewWindowList.Create;
end;

destructor TEditorManager.Destroy;
begin
  FTabs.Free;
  FViewWindows.Free;
  inherited Destroy;
end;

end.
