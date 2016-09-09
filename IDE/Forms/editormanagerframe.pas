{ TODO : Organize Tabs with a List
Fenster Views }
unit EditorManagerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, ComCtrls, Editor, FormEditor,
  Dialogs, Buttons, Menus, ExtCtrls, au3Types, Project, fgl;

type
  TCloseEditorEvent = procedure(Sender: TObject; Editor: integer;
    var Proceed: boolean) of object;
  TEditorNotifyEvent = procedure(Sender: TObject; Editor: integer) of object;

  TEditorList = specialize TFPGList<TTabSheet>;

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
    procedure BLMoveMenuClick(Sender: TObject);
    procedure BRMoveMenuClick(Sender: TObject);
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
    procedure TRMoveMenuClick(Sender: TObject);
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
    { Functions & Procedures }
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
    procedure CreateEditor(FName: string; Line, Pos: integer);
    procedure EditorChanged(Sender: TObject);
    function GetFileName(i: integer): string;
    procedure SetFileName(i: integer; s: string);
    function GetCount: integer;
    function GetEditorCaret(i: integer): TPoint;
    procedure SetEditorCaret(i: integer; p: TPoint);
    function FindIndex(T: TTabSheet): integer;
  public
    function OpenEditor(FileName: string; Pos: TPoint): TFrame;
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
  TRMoveMenu.Tag := c.Tag;
  TLMoveMenu.Tag := c.Tag;
  BRMoveMenu.Tag := c.Tag;
  BLMoveMenu.Tag := c.Tag;
  TRMoveMenu.Visible := SplitTopButton.Down;
  BLMoveMenu.Visible := SplitVButton.Down;
  BRMoveMenu.Visible := SplitVButton.Down and SplitBotButton.Down;
  p := c.Parent.ClientToScreen(Point(c.Left, c.Top + c.Height));
  case c.Tag of
    0: e := EditorControl;
    1: e := EditorControl1;
    2: e := EditorControl2;
    3: e := EditorControl3;
  end;
  if Assigned(e.ActivePage) then
    MoveToPopup.PopUp(p.x, p.y);
end;

procedure TEditorManager.NewWindowMoveMenuClick(Sender: TObject);
var
  p: TMenuItem;
begin
  p := (Sender as TMenuItem);
  case p.tag of
    0: EditorControl.ActivePage.ManualDock(nil);
    1: EditorControl1.ActivePage.ManualDock(nil);
    2: EditorControl2.ActivePage.ManualDock(nil);
    3: EditorControl3.ActivePage.ManualDock(nil);
  end;
end;

procedure TEditorManager.OpenEditorButton1Click(Sender: TObject);
begin

end;

procedure TEditorManager.SplitBotButtonClick(Sender: TObject);
begin
  if not SplitBotButton.Down then
    while EditorControl3.PageCount > 0 do
      EditorControl3.Pages[0].PageControl := EditorControl2;
  RPanelBot.Width := Self.Width div 2;
  RPanelBot.Visible := SplitBotButton.Down;
  LRSplitterBot.Visible := SplitBotButton.Down;
end;

procedure TEditorManager.SplitTopButtonClick(Sender: TObject);
begin
  if not SplitTopButton.Down then
    while EditorControl1.PageCount > 0 do
      EditorControl1.Pages[0].PageControl := EditorControl;
  RPanelTop.Width := Self.Width div 2;
  RPanelTop.Visible := SplitTopButton.Down;
  LRSplitterTop.Visible := SplitTopButton.Down;
end;

procedure TEditorManager.SplitVButtonClick(Sender: TObject);
begin
  if not SplitVButton.Down then
  begin
    while EditorControl2.PageCount > 0 do
      EditorControl2.Pages[0].PageControl := EditorControl;
    while EditorControl3.PageCount > 0 do
      EditorControl3.Pages[0].PageControl := EditorControl;
  end;
  BotPanel.Height := Self.Height div 2;
  BotPanel.Visible := SplitVButton.Down;
  SplitBotButton.Visible := SplitVButton.Down;
  TopBotSplit.Visible := SplitVButton.Down;
  SplitBotButton.Left := 0;
  SplitVButton.Left := 0;
  SplitTopButton.Left := 0;
end;

procedure TEditorManager.TLMoveMenuClick(Sender: TObject);
var
  p: TMenuItem;
begin
  p := (Sender as TMenuItem);
  case p.tag of
    0: EditorControl.ActivePage.PageControl := (EditorControl);
    1: EditorControl1.ActivePage.PageControl := (EditorControl);
    2: EditorControl2.ActivePage.PageControl := (EditorControl);
    3: EditorControl3.ActivePage.PageControl := (EditorControl);
  end;
end;

procedure TEditorManager.TRMoveMenuClick(Sender: TObject);
var
  p: TMenuItem;
begin
  p := (Sender as TMenuItem);
  case p.tag of
    0: EditorControl.ActivePage.PageControl := (EditorControl1);
    1: EditorControl1.ActivePage.PageControl := (EditorControl1);
    2: EditorControl2.ActivePage.PageControl := (EditorControl1);
    3: EditorControl3.ActivePage.PageControl := (EditorControl1);
  end;
end;

procedure TEditorManager.EditorEnter(Sender: TObject);
begin
  FFocused := FindIndex(Sender as TTabSheet);
end;

procedure TEditorManager.EditorControlChange(Sender: TObject);
begin
  if Assigned(FOnEditorChanged) then
    FOnEditorChanged(Self);
end;

procedure TEditorManager.BLMoveMenuClick(Sender: TObject);
var
  p: TMenuItem;
begin
  p := (Sender as TMenuItem);
  case p.tag of
    0: EditorControl.ActivePage.PageControl := (EditorControl2);
    1: EditorControl1.ActivePage.PageControl := (EditorControl2);
    2: EditorControl2.ActivePage.PageControl := (EditorControl2);
    3: EditorControl3.ActivePage.PageControl := (EditorControl2);
  end;
end;

procedure TEditorManager.BRMoveMenuClick(Sender: TObject);
var
  p: TMenuItem;
begin
  p := (Sender as TMenuItem);
  case p.tag of
    0: EditorControl.ActivePage.PageControl := (EditorControl3);
    1: EditorControl1.ActivePage.PageControl := (EditorControl3);
    2: EditorControl2.ActivePage.PageControl := (EditorControl3);
    3: EditorControl3.ActivePage.PageControl := (EditorControl3);
  end;
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
    if ((FTabs[i].Components[0] is TEditorFrame) and
      ((FTabs[i].Components[0] as TEditorFrame).FileName = FileName)) or
      ((FTabs[i].Components[0] is TFormEditFrame) and
      ((FTabs[i].Components[0] as TFormEditFrame).FileName = FileName)) then
    begin
      Result := i;
      Exit;
    end;
end;

function TEditorManager.GetEditor(i: integer): TFrame;
begin
  if (i >= 0) and (i < Count) then
    Result := FTabs[i].Components[0] as TFrame
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
    if FTabs[i].Components[0] = f then
    begin
      if Assigned(FTabs[i].PageControl) then
        FTabs[i].PageControl.ActivePage := FTabs[i];
      FTabs[i].SetFocus;
      Break;
    end;
end;

function TEditorManager.GetIndex: integer;
begin
  Result := FFocused;
end;

procedure TEditorManager.SetIndex(i: integer);
begin
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

procedure TEditorManager.CreateEditor(FName: string; Line, Pos: integer);
var
  tmp: TTabSheet;
  ext: string;
begin
  tmp := EditorControl.AddTabSheet;
  tmp.Caption := ExtractFileName(FName);
  tmp.Visible := True;
  FFocused := FTabs.Add(tmp);
  EditorControl.ActivePage := tmp;
  ext := ExtractFileExt(FName);
  if ext = '.afm' then
    with TFormEditFrame.Create(tmp) do
    begin
      Align := alClient;
      Parent := tmp;
      Visible := True;
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
    with TEditorFrame.Create(tmp) do
    begin
      Align := alClient;
      Parent := tmp;
      Visible := True;
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

function TEditorManager.OpenEditor(FileName: string; Pos: TPoint): TFrame;
var
  Index: integer;
begin
  Index := FindEditor(FileName);
  if Index = -1 then
    CreateEditor(FileName, Pos.y, Pos.x)
  else
  begin
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
  if not Proceed then
    Exit;
  FTabs[i].Components[0].Free;
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
end;

destructor TEditorManager.Destroy;
begin
  FTabs.Free;
  inherited Destroy;
end;

end.
