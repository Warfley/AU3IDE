unit EditorManagerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, ComCtrls, Editor, FormEditor,
  Dialogs, au3Types, Project;

type
  TCloseEditorEvent = procedure(Sender: TObject; Editor: integer;
    var Proceed: boolean) of object;
  TEditorNotifyEvent = procedure(Sender: TObject; Editor: integer) of object;

  { TEditorManager }

  TEditorManager = class(TFrame)
    EditorControl: TPageControl;
    procedure EditorControlChange(Sender: TObject);
    procedure EditorControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    { Fields }
    FProject: Tau3Project;
    FIncludePath: string;
    FOnEditorClose: TCloseEditorEvent;
    FOnEditorCreated: TEditorNotifyEvent;
    FOnEditorChanged: TNotifyEvent;
    FOpenEditor: TOpenEditorEvent;
    FEnterFunc: TOpenFunctionEvent;
    FOnParserFinished: TNotifyEvent;
    { Functions & Procedures }
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
begin
  if Button = mbMiddle then
    CloseEditor(EditorControl.TabIndexAtClientPos(Point(X, Y)));
end;

procedure TEditorManager.EditorControlChange(Sender: TObject);
begin
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
  for i := 0 to EditorControl.PageCount - 1 do
    if ((EditorControl.Pages[i].Components[0] is TEditorFrame) and
      ((EditorControl.Pages[i].Components[0] as TEditorFrame).FileName = FileName)) or
      ((EditorControl.Pages[i].Components[0] is TFormEditFrame) and
      ((EditorControl.Pages[i].Components[0] as TFormEditFrame).FileName =
      FileName)) then
    begin
      Result := i;
      Exit;
    end;
end;

function TEditorManager.GetEditor(i: integer): TFrame;
begin
  Result := EditorControl.Pages[i].Components[0] as TFrame;
end;

function TEditorManager.GetCurrentEditor: TFrame;
begin
  if Assigned(EditorControl.ActivePage) then
    Result := EditorControl.ActivePage.Components[0] as TFrame
  else
    Result := nil;
end;

procedure TEditorManager.SetCurrentEditor(f: TFrame);
var
  i: integer;
begin
  for i := 0 to EditorControl.PageCount - 1 do
    if EditorControl.Pages[i].Components[0] = f then
      EditorControl.PageIndex := i;
end;

function TEditorManager.GetIndex: integer;
begin
  Result := EditorControl.PageIndex;
end;

procedure TEditorManager.SetIndex(i: integer);
begin
  EditorControl.PageIndex := i;
  if CurrentEditor is TEditorFrame then
    (CurrentEditor as TEditorFrame).SetFocus;
end;

procedure TEditorManager.EditorChanged(Sender: TObject);
begin
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
  Result := EditorControl.PageCount;
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

procedure TEditorManager.CreateEditor(FName: string; Line, Pos: integer);
var
  tmp: TTabSheet;
  ext: string;
begin
  tmp := EditorControl.AddTabSheet;
  tmp.Caption := ExtractFileName(FName);
  tmp.Visible := True;
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
      if FName=GetFullPath(FProject.MainForm, IncludePath,FProject.ProjectDir,FProject.Paths) then
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
      IncludePath:=FIncludePath;
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
    EditorControl.PageIndex := Index;
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
  EditorControl.Pages[i].Components[0].Free;
  EditorControl.Pages[i].Free;
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

end.
