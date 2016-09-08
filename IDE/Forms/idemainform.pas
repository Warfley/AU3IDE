unit IDEMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls,
  Menus, ComCtrls, Buttons, ExtCtrls, PairSplitter, Project, IDEStartupScreen,
  ProjectInspector, EditorManagerFrame, au3Types, FormEditor, Editor,
  au3FileInfo, strutils, CompilerOptions, au3Compiler, EditorOptions, FormEditorOptions,
  SampeProjectView, fphttpclient, process, AboutWindow, Math, aboutautoit;

type

  { TMainForm }
  PEnterFuncInfo = ^TEnterFuncInfo;

  TEnterfuncInfo = record
    FileName: string;
    Pos: TPoint;
  end;

  TIDEState = record
    Left,
    Top,
    Width,
    Height: integer;
    State: TWindowState;
    PILeft: boolean;
  end;

  PCreateFuncInfo = ^TCreateFuncInfo;

  TCreateFuncInfo = record
    FileName, Func: string;
  end;

  TMainForm = class(TForm)
    au3IDEProps: TApplicationProperties;
    FormOptionsItem: TMenuItem;
    IDEOptionItem: TMenuItem;
    ExtrasMenuItem: TMenuItem;
    CompileMenuItem: TMenuItem;
    AboutAutoitItem: TMenuItem;
    NextTabItem: TMenuItem;
    CenterPanel: TPanel;
    PrevTabItem: TMenuItem;
    MenuSplitItem6: TMenuItem;
    ProjectInspector1: TProjectInspector;
    RunMenuItem: TMenuItem;
    OutBoxSplitter: TSplitter;
    ProjectExplorerSplitter: TSplitter;
    Splitter2: TSplitter;
    UpdateMenuItem: TMenuItem;
    InfoMenuItem: TMenuItem;
    SampeButton: TMenuItem;
    TextEditorOptionsItem: TMenuItem;
    OutputBox: TListBox;
    SaveAllBtn: TSpeedButton;
    CloseAllBtn: TSpeedButton;
    CloseEditorBtn: TSpeedButton;
    SelectModeBox: TComboBox;
    EditorManager1: TEditorManager;
    MainFormMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    CloseFileItem: TMenuItem;
    CloseAllItem: TMenuItem;
    EditMenuItem: TMenuItem;
    FormatMenuItem: TMenuItem;
    ConfigMenuItem: TMenuItem;
    CompOptionMenuItem: TMenuItem;
    ToolbarSplit1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    RunMenu: TMenuItem;
    Openau3FileDialog: TOpenDialog;
    SaveAsItem: TMenuItem;
    Saveau3FileDialog: TSaveDialog;
    SaveFileItem: TMenuItem;
    SaveAllItem: TMenuItem;
    MenuSplitItem5: TMenuItem;
    MenuSplitItem4: TMenuItem;
    MenuSplitItem3: TMenuItem;
    SearchMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    AddUnitBtn: TSpeedButton;
    AddFormBtn: TSpeedButton;
    SaveAsBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    NewProjBtn: TSpeedButton;
    RunBtn: TSpeedButton;
    StopBtn: TSpeedButton;
    MainToolbar: TToolBar;
    UndoMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    MenuSplitItem2: TMenuItem;
    MenuSplitItem1: TMenuItem;
    NewFormItem: TMenuItem;
    NewFileItem: TMenuItem;
    NewMenuItem: TMenuItem;
    NewProjectItem: TMenuItem;
    procedure AboutAutoitItemClick(Sender: TObject);
    procedure CloseAllItemClick(Sender: TObject);
    procedure CloseFileItemClick(Sender: TObject);
    procedure CompileMenuItemClick(Sender: TObject);
    procedure CompOptionMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormOptionsItemClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure IDEOptionItemClick(Sender: TObject);
    procedure InfoMenuItemClick(Sender: TObject);
    procedure NewFileItemClick(Sender: TObject);
    procedure NewFormItemClick(Sender: TObject);
    procedure NewProjectItemClick(Sender: TObject);
    procedure NextTabItemClick(Sender: TObject);
    procedure PrevTabItemClick(Sender: TObject);
    procedure RedoMenuItemClick(Sender: TObject);
    procedure RunBtnClick(Sender: TObject);
    procedure RunMenuItemClick(Sender: TObject);
    procedure SampeButtonClick(Sender: TObject);
    procedure SaveAllItemClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure SaveFileItemClick(Sender: TObject);
    procedure KillEditor(s: string);
    procedure SelectModeBoxChange(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure TextEditorOptionsItemClick(Sender: TObject);
    procedure MainToolbarPaint(Sender: TObject);
    procedure UndoMenuItemClick(Sender: TObject);
    procedure UpdateMenuItemClick(Sender: TObject);
  private
    FFirstLoad: boolean;
    FSaveOnClosing: boolean;
    FCompiler: Tau3Compiler;
    FFormIsClosing: boolean;
    FCurrentProject: Tau3Project;
    FLastOpend: TStringList;
    FFileData: Tau3FileManager;
    FCurrentState: TIDEState;
    IncludePath: string;
    { private declarations }
    procedure OpenProject(P: string);
    function ShowFormConf: boolean;
    procedure EditorParserFinished(Sender: TObject);
    procedure ShowStartupScreen(Data: IntPtr);
    procedure OpenFile(Filename: string; Pos: TPoint);
    function EnterFunction(FileName, FuncName: string; Params: string;
      CreateIfMissing: boolean): string;
    procedure AddInclude(FileName, IncludeFile: string);
    function CheckInclude(FileName, IncludeFile: string): boolean;
    procedure EditorClosing(Sender: TObject; Editor: integer; var Proceed: boolean);
    procedure EditorCreated(Sender: TObject; Editor: integer);
    procedure EditorChanged(Sender: TObject);
    procedure UpdateProject(Data: IntPtr);
    procedure EnterFunc(Data: IntPtr);
    procedure CreateFunc(Data: IntPtr);
    procedure ChangeMainForm(FileName: string; Silent: boolean = False);
    function ShowCompilerOptions: boolean;
    function ShowEditorConf: boolean;
    procedure PrintText(Sender: TObject; FileName: string; Output: string);
    procedure FinishedComp(Sender: TObject);
    procedure FinishedRun(Sender: TObject);
    procedure CompileError(Sender: TObject);
    function CheckForUpdates: boolean;
    procedure PerformUpdate;
  public
    property CurrentProject: Tau3Project read FCurrentProject;
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

function TMainForm.CheckForUpdates: boolean;
var
  sl: TStringList;
  f: TFPHTTPClient;
begin
  sl := TStringList.Create;
  f := TFPHTTPClient.Create(nil);
  try
    sl.Text := f.Get(SUpdateURL + 'Update.txt');
    Result := sl[0] <> Version;
  finally
    f.Free;
    sl.Free;
  end;
end;

procedure TMainForm.PerformUpdate;
var
  p: TProcess;
  c: TCloseAction;
begin
  c := caNone;
  FormClose(Self, c);
  if EditorManager1.Count > 0 then
    exit;
  FCurrentProject.Clear;
  p := TProcess.Create(nil);
  try
    p.Executable := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'Updater.exe';
    p.Execute;
  finally
    p.Free;
  end;
  Close;
end;

procedure TMainForm.OpenProject(P: string);
var
  c: TCloseAction;
begin
  if FileExists(P) and (ExtractFileExt(P) = '.au3proj') then
  begin
    StartupScreen.SelectedPath := P;
    c := caNone;
    FormClose(Self, c);
    if EditorManager1.Count > 0 then
      exit;
    FCurrentProject.Clear;
    Application.QueueAsyncCall(@ShowStartupScreen, 1);
  end;
end;

function TMainForm.ShowFormConf: boolean;
var
  i: integer;
begin
  FormEditorOptionsForm.Load(IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0))) + 'foms.cfg');
  Result := False;
  if FormEditorOptionsForm.ShowModal = mrOk then
  begin
    Result := True;
    FormEditorOptionsForm.Save(IncludeTrailingPathDelimiter(
      ExtractFilePath(ParamStr(0))) + 'foms.cfg');
    for i := 0 to EditorManager1.Count - 1 do
      if EditorManager1.Editors[i] is TFormEditFrame then
        (EditorManager1.Editors[i] as TFormEditFrame).ReLoadConf;
  end;
end;

function TMainForm.ShowEditorConf: boolean;
var
  i: integer;
begin
  EditorConf.Load(ExtractFilePath(ParamStr(0)));
  Result := False;
  if EditorConf.ShowModal = mrOk then
  begin
    Result := True;
    EditorConf.Save(ExtractFilePath(ParamStr(0)));
    for i := 0 to EditorManager1.Count - 1 do
      if EditorManager1.Editors[i] is TEditorFrame then
        (EditorManager1.Editors[i] as TEditorFrame).ReLoadConf;
  end;
end;

procedure TMainForm.PrintText(Sender: TObject; FileName: string; Output: string);
begin
  OutputBox.Items.Add(FCurrentProject.GetRelPath(FileName) + ': ' + Output);
  OutputBox.ItemIndex := OutputBox.Items.Count - 1;
  OutputBox.ItemIndex := -1;
end;

procedure TMainForm.FinishedComp(Sender: TObject);
begin
  OutputBox.Items.Add('Kompilieren abgeschlossen');
  OutputBox.ItemIndex := OutputBox.Items.Count - 1;
  OutputBox.ItemIndex := -1;
end;

procedure TMainForm.FinishedRun(Sender: TObject);
begin
  OutputBox.Items.Add('Ausführung beendet');
  OutputBox.ItemIndex := OutputBox.Items.Count - 1;
  OutputBox.ItemIndex := -1;
  RunBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

procedure TMainForm.CompileError(Sender: TObject);
begin
  OutputBox.Items.Add('Compiler Fehler, ausführung beendet');
  OutputBox.ItemIndex := OutputBox.Items.Count - 1;
  OutputBox.ItemIndex := -1;
  RunBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

function TMainForm.ShowCompilerOptions: boolean;
begin
  Result := False;
  CompilerOptionsForm.PathEdit.Directory := FCompiler.Path;
  CompilerOptionsForm.SaveIntBox.Checked := FCompiler.SaveIntData;
  if CompilerOptionsForm.ShowModal = mrOk then
  begin
    FCompiler.Path := CompilerOptionsForm.PathEdit.Directory;
    FCompiler.SaveIntData := CompilerOptionsForm.SaveIntBox.Checked;
    FCompiler.WriteConf(IncludeTrailingPathDelimiter(
      ExtractFilePath(ParamStr(0))) + 'compiler.cfg');
    IncludePath := IncludeTrailingPathDelimiter(FCompiler.Path) + 'Include';
    EditorManager1.IncludePath := IncludePath;
    Result := True;
  end;
end;

procedure TMainForm.OpenFile(Filename: string; Pos: TPoint);
begin
  if not FilenameIsAbsolute(Filename) then
    FileName := FCurrentProject.GetAbsPath(Filename);
  EditorManager1.OpenEditor(Filename, Pos);
end;

procedure TMainForm.ShowStartupScreen(Data: IntPtr);

  procedure StringsDelete(s: TStrings; str: string);
  var
    i: integer;
  begin
    for i := 0 to s.Count - 1 do
      if s[i] = str then
      begin
        s.Delete(i);
        Break;
      end;
  end;

var
  i: integer;
begin
  Self.Hide;
  if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'compiler.cfg') then
    FCompiler.ReadConf(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'compiler.cfg')
  else if not ShowCompilerOptions then
  begin
    Close;
    Exit;
  end;
  IncludePath := IncludeTrailingPathDelimiter(FCompiler.Path) + 'Include';
  EditorManager1.IncludePath := IncludePath;
  StartupScreen.LastOpend := FLastOpend;
  if FFirstLoad and (Paramcount > 0) and FileExists(ParamStr(1)) and
    (LowerCase(ExtractFileExt(ParamStr(1))) = '.au3proj') then
    StartupScreen.SelectedPath := ParamStr(1)
  else if Data <> 1 then
    StartupScreen.ShowModal;
  FFirstLoad := False;
  if FileExists(StartupScreen.SelectedPath) then
  begin
    StringsDelete(FLastOpend, StartupScreen.SelectedPath);
    FLastOpend.Insert(0, StartupScreen.SelectedPath);
    FCurrentProject.ReadFromFile(StartupScreen.SelectedPath);
    FCurrentProject.CheckInclude := @CheckInclude;
    FCurrentProject.AddInclude := @AddInclude;
    EditorManager1.Project := FCurrentProject;
    ProjectInspector1.Project := FCurrentProject;
    Openau3FileDialog.InitialDir := FCurrentProject.ProjectDir;
    Saveau3FileDialog.InitialDir := FCurrentProject.ProjectDir;
    Self.Show;
    for i := 0 to FCurrentProject.OpendFiles.Count - 1 do
      if FileExists(FCurrentProject.GetAbsPath(
        FCurrentProject.OpendFiles[i].Name)) then
        EditorManager1.OpenEditor(FCurrentProject.GetAbsPath(
          FCurrentProject.OpendFiles[i].Name),
          Point(FCurrentProject.OpendFiles[i].Pos, FCurrentProject.OpendFiles[i].Line));
    EditorManager1.EditorIndex := FCurrentProject.FocusedFile;
    ProjectInspector1.OpenEditor := @OpenFile;
    ProjectInspector1.CloseEditor := @KillEditor;
    EditorManager1.OnEditorClose := @EditorClosing;
    EditorManager1.OnEditorChanged := @EditorChanged;
    EditorManager1.OnEditorCreated := @EditorCreated;
    EditorManager1.IncludePath := IncludePath;
  end
  else
    Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  mr: TModalResult;
  f: file of TIDEState;
  fb: file of boolean;
begin
  FSaveOnClosing := False;
  if FCompiler.Active then
    if MessageDlg('Ausführung noch nicht beendet',
      'Der Compiler oder Interpreter ist noch aktiv, soll der Prozess gestoppt werden?',
      mtConfirmation, [mbYes, mbCancel], 'Beenden?') = mrYes then
      FCompiler.Stop
    else
    begin
      CloseAction := caNone;
      Exit;
    end;
  FFormIsClosing := True;
  EditorManager1.OnEditorChanged := nil;
  EditorManager1.OnEditorCreated := nil;
  CloseAllItemClick(Sender);
  if EditorManager1.Count > 0 then
    CloseAction := caNone
  else
  if FSaveOnClosing then
    FCurrentProject.Save
  else
  if FCurrentProject.Changed then
  begin
    mr := MessageDlg('Projekt sichern',
      'Das Projekt hat sich seit dem Letzten öffnen geändert'#10#13'Projekt sichern?',
      mtConfirmation, mbYesNoCancel, 'Sichern');
    case mr of
      mrYes: FCurrentProject.Save;
      mrAbort: CloseAction := caNone;
    end;
  end;
  if CloseAction = caNone then
  begin
    EditorManager1.OnEditorChanged := @EditorChanged;
    EditorManager1.OnEditorCreated := @EditorCreated;
    FFormIsClosing := False;
    FSaveOnClosing := True;
  end;
  FLastOpend.SaveToFile(ExtractFilePath(ParamStr(0)) + 'LastOpend.txt');

  FCurrentState.Left := Left;
  FCurrentState.Top := Top;
  AssignFile(f, IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'wnd.cnf');
  try
    Rewrite(f);
    Write(f, FCurrentState);
  finally
    CloseFile(f);
  end;
end;

procedure TMainForm.CloseFileItemClick(Sender: TObject);
begin
  EditorManager1.CloseEditor(EditorManager1.EditorIndex);
end;

procedure TMainForm.CompileMenuItemClick(Sender: TObject);
begin
  SaveAllItemClick(nil);
  OutputBox.Clear;
  if SelectModeBox.ItemIndex = 0 then
    FCompiler.Compile(FCurrentProject, cax86)
  else
    FCompiler.Compile(FCurrentProject, ca64);
end;

procedure TMainForm.CompOptionMenuItemClick(Sender: TObject);
begin
  ShowCompilerOptions;
end;

procedure TMainForm.CloseAllItemClick(Sender: TObject);
var
  i: integer;
begin
  i := EditorManager1.Count + 1;
  while (EditorManager1.Count > 0) and (i > EditorManager1.Count) do
  begin
    i := EditorManager1.Count;
    EditorManager1.CloseEditor(EditorManager1.EditorIndex);
    EditorManager1.Invalidate;
  end;
end;

procedure TMainForm.AboutAutoitItemClick(Sender: TObject);
begin
  AboutAutoitForm.ShowModal;
end;

procedure TMainForm.EnterFunc(Data: IntPtr);
begin
  EditorManager1.OpenEditor(PEnterFuncInfo(Data)^.FileName,
    PEnterFuncInfo(Data)^.Pos);
  Dispose(PEnterFuncInfo(Data));
end;


procedure TMainForm.CreateFunc(Data: IntPtr);
var
  e: TEditorFrame;
begin
  with PCreateFuncInfo(Data)^ do
  begin
    e := EditorManager1.OpenEditor(FileName, Point(0, 0)) as TEditorFrame;
    e.CodeEditor.TextBetweenPoints[Point(
      Length(e.CodeEditor.Lines[e.CodeEditor.Lines.Count - 1]) + 1,
      e.CodeEditor.Lines.Count),
      Point(Length(e.CodeEditor.Lines[e.CodeEditor.Lines.Count - 1]) +
      1, e.CodeEditor.Lines.Count)] := #13#13 + 'Func ' + Func + #13#13 + 'EndFunc';
    e.CodeEditor.LogicalCaretXY := Point(2, e.CodeEditor.Lines.Count - 1);
    Application.QueueAsyncCall(@e.MoveHorz, 2);
  end;
  Dispose(PCreateFuncInfo(Data));
end;

function TMainForm.EnterFunction(FileName, FuncName: string; Params: string;
  CreateIfMissing: boolean): string;
var
  i: integer;
  e: TEditorFrame;
  d: PEnterFuncInfo;
  c: PCreateFuncInfo;
begin
  if FFileData.FileIndex[FileName] = -1 then
  begin
    FFileData.LoadFile(FileName);
    exit;
  end;
  with FFileData[FFileData.FileIndex[FileName]] do
    for i := 0 to Functions.Count - 1 do
      if pos(LowerCase(FuncName), LowerCase(Functions[i].Name)) = 1 then
      begin
        new(d);
        d^.FileName := FileName;
        d^.Pos := Point(1, Functions[i].Line + 2);
        Application.QueueAsyncCall(@EnterFunc, PtrInt(d));
        Exit;
      end;
  if CreateIfMissing then
  begin
    new(c);
    c^.FileName := FileName;
    c^.Func := FuncName + '(' + Params + ')';
    Application.QueueAsyncCall(@CreateFunc, IntPtr(c));
  end;
end;

procedure TMainForm.AddInclude(FileName, IncludeFile: string);
var
  e: TEditorFrame;
  sl: TStringList;
begin
  if FilenameIsAbsolute(IncludeFile) then
    IncludeFile := GetRelInclude(IncludeFile, IncludePath,
      ExtractFilePath(FileName), FCurrentProject.Paths);
  e := EditorManager1.TextEditor[FileName];
  if Assigned(e) then
    e.CodeEditor.TextBetweenPoints[Point(1, 1), Point(1, 1)] :=
      Format('#include<%s>'#13, [IncludeFile])
  else if FileExists(FileName) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(FileName);
      sl.Insert(0, Format('#include<%s>', [IncludeFile]));
      sl.SaveToFile(FileName);
    finally
      sl.Free;
    end;
  end;
end;

function TMainForm.CheckInclude(FileName, IncludeFile: string): boolean;

  function ExtractBetween(const Value, A, B: string): string;
  var
    aPos, bPos: integer;
  begin
    Result := '';
    aPos := Pos(A, Value);
    if aPos > 0 then
    begin
      aPos := aPos + Length(A);
      bPos := PosEx(B, Value, aPos);
      if bPos > 0 then
      begin
        Result := Copy(Value, aPos, bPos - aPos);
      end;
    end;
  end;

var
  i: integer;
  e: TEditorFrame;
  sl: TStringList;
  fname: string;
begin
  e := EditorManager1.TextEditor[FileName];
  Result := False;
  ;
  if Assigned(e) then
  begin
    i := 0;
    while i < e.CodeEditor.Lines.Count do
    begin
      if isEnd(e.CodeEditor.Lines[i], '#include') then
      begin
        if pos('<', e.CodeEditor.Lines[i]) > 0 then
          fname := ExtractBetween(e.CodeEditor.Lines[i], '<', '>')
        else
          fname := ExtractBetween(e.CodeEditor.Lines[i], '"', '"');
        Result := IncludeFile = fname;
        if not FilenameIsAbsolute(fname) then
          fname := GetFullPath(fname, IncludePath, ExtractFilePath(FileName),
            FCurrentProject.Paths);
        Result := Result or (IncludeFile = fname);
        if not (FileExists(fname) or (EditorManager1.Editor[fname] >= 0)) then
        begin
          if i = e.CodeEditor.Lines.Count - 1 then
            e.CodeEditor.TextBetweenPoints[Point(1, i + 1),
              Point(Length(e.CodeEditor.Lines[i]) + 1, i + 1)] := ''
          else
            e.CodeEditor.TextBetweenPoints[Point(1, i + 1), Point(1, i + 2)] := '';
          Continue;
        end;
      end;
      Inc(i);
    end;
  end
  else if FileExists(FileName) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(FileName);
      i := 0;
      while i < sl.Count - 1 do
      begin
        if isEnd(sl[i], '#include') then
        begin
          if pos('<', sl[i]) > 0 then
            fname := ExtractBetween(sl[i], '<', '>')
          else
            fname := ExtractBetween(sl[i], '"', '"');
          Result := IncludeFile = fname;
          if not FilenameIsAbsolute(fname) then
            fname := GetFullPath(fname, IncludePath, ExtractFilePath(FileName),
              FCurrentProject.Paths);
          Result := Result or (IncludeFile = fname);
          if not (FileExists(fname) or (EditorManager1.Editor[fname] >= 0)) then
          begin
            sl.Delete(i);
            Continue;
          end;
        end;
        Inc(i);
      end;
      sl.SaveToFile(FileName);
    finally
      sl.Free;
    end;
  end;
end;

procedure TMainForm.EditorClosing(Sender: TObject; Editor: integer;
  var Proceed: boolean);
var
  res: TModalResult;
begin
  if EditorManager1.Editors[Editor].Parent.Caption[1] = '*' then
  begin
    EditorManager1.EditorIndex := Editor;
    EditorManager1.Invalidate;
    res := MessageDlg('Datei wurde Verändert',
      'Datei wurde Verändert'#10#13'Vor dem schließen Sichern?',
      mtConfirmation, mbYesNoCancel, 'Schließen?');
    case res of
      mrYes: if FFormIsClosing then
        begin
          SaveAllItemClick(SaveAllItem);
          FSaveOnClosing := True;
        end
        else
          SaveFileItemClick(SaveFileItem);
      mrNo: Exit;
      mrCancel: Proceed := False;
    end;
  end;
  if Proceed and not FFormIsClosing then
    Application.QueueAsyncCall(@UpdateProject, 0);
end;

procedure TMainForm.EditorCreated(Sender: TObject; Editor: integer);
begin
  Application.QueueAsyncCall(@UpdateProject, 0);
end;

procedure TMainForm.EditorChanged(Sender: TObject);
begin
  Application.QueueAsyncCall(@UpdateProject, 0);
end;

procedure TMainForm.UpdateProject(Data: IntPtr);
var
  i: integer;
begin
  FCurrentProject.OpendFiles.Clear;
  for i := 0 to EditorManager1.Count - 1 do
    FCurrentProject.OpendFiles.Add(
      OpendFileInfo(FCurrentProject.GetRelPath(EditorManager1.EditorFiles[i]),
      EditorManager1.EditorCaret[i].Y, EditorManager1.EditorCaret[i].X));
  FCurrentProject.FocusedFile := EditorManager1.EditorIndex;
  FCurrentProject.Changed := True;
end;

procedure TMainForm.KillEditor(s: string);
var
  i: integer;
begin
  for i := 0 to EditorManager1.Count - 1 do
    if EditorManager1.EditorFiles[i] = s then
    begin
      EditorManager1.Editors[i].Free;
      EditorManager1.EditorControl.Pages[i].Free;
      Break;
    end;
end;

procedure TMainForm.SelectModeBoxChange(Sender: TObject);
var
  HotKey: string;
begin
  if SelectModeBox.ItemIndex = 0 then
    HotKey := 'F5'
  else
    HotKey := 'Strg + R';
  RunBtn.Hint := Format('Ausführen %s (%s)',
    [SelectModeBox.Items[SelectModeBox.ItemIndex], Hotkey]);
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
begin
  FCompiler.Stop;
  RunBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

procedure TMainForm.TextEditorOptionsItemClick(Sender: TObject);
begin
  ShowEditorConf;
end;

procedure TMainForm.MainToolbarPaint(Sender: TObject);
begin
  MainToolbar.Canvas.Pen.Style := psSolid;
  MainToolbar.Canvas.Pen.Color := $00DEDEDE;
  MainToolbar.Canvas.Pen.Width := 2;
  MainToolbar.Canvas.MoveTo(-1, MainToolbar.Height - 1);
  MainToolbar.Canvas.LineTo(MainToolbar.Width, MainToolbar.Height - 1);
end;

procedure TMainForm.UndoMenuItemClick(Sender: TObject);
begin
  { TODO : Use Interface instead of this bullshit }
  if EditorManager1.CurrentEditor is TFormEditFrame then
    (EditorManager1.CurrentEditor as TFormEditFrame).DoUndo;
end;

procedure TMainForm.UpdateMenuItemClick(Sender: TObject);
begin
  if CheckForUpdates then
  begin
    if MessageDlg('Neue Version gefunden',
      'Eine neue Version Steht zum Download verfügbar.'#10#13'Jetzt aktualisieren?',
      mtInformation, mbYesNo, 'Update') = mrYes then
      PerformUpdate;
  end
  else
    ShowMessage('Keine neue Version gefunden');
end;

procedure TMainForm.EditorParserFinished(Sender: TObject);

  procedure AddReq(req: string; sl: TStringList);
  var
    f, n: integer;
  begin
    { Anpassen an PATH variablen }
    if not FilenameIsAbsolute(req) then
      req := GetFullPath(req, IncludePath, ExtractFilePath(
        (Sender as TEditorFrame).FileName) + PathDelim, FCurrentProject.Paths);
    if StringsContain(sl, req) then
      exit;
    sl.Add(req);
    f := FFileData.FileIndex[req];
    if f >= 0 then
    begin
      for n := 0 to FFileData[f].Variables.Count - 1 do
        (Sender as TEditorFrame).VariableList.Add(FFileData[f].Variables[n]);
      for n := 0 to FFileData[f].Functions.Count - 1 do
        (Sender as TEditorFrame).FunctionList.Add(FFileData[f].Functions[n]);
      for n := 0 to FFileData[f].RequiredFiles.Count - 1 do
        AddReq(FFileData[f].RequiredFiles[n], sl);
    end
    else
      FFileData.LoadFile(req);
  end;

var
  i, idx: integer;
  e: TFormEditFrame;
  sl: TStringList;
begin
  if Sender is TFormEditFrame then
  begin
    idx := FFileData.FileIndex[(Sender as TFormEditFrame).FileName];
    if idx = -1 then
      idx := FFileData.CreateFile((Sender as TFormEditFrame).FileName);
    (Sender as TFormEditFrame).AddToVarlist(FFileData[idx].Variables);
    idx := FFileData.FileIndex[ChangeFileExt(
      (Sender as TFormEditFrame).FileName, 'au3')];
    if idx = -1 then
      idx := FFileData.LoadFile(ChangeFileExt(
        (Sender as TFormEditFrame).FileName, 'au3'));
  end
  else if Sender is TEditorFrame then
  begin
    e := EditorManager1.FormEditor[ChangeFileExt(
      (Sender as TEditorFrame).FileName, '.afm')];
    if Assigned(e) then
    begin
      with Sender as TEditorFrame do
      begin
        e.FuncList.Clear;
        for i := 0 to FunctionList.Count - 1 do
        begin
          e.FuncList.Add(Copy(FunctionList[i].Name, 1,
            Pos('(', FunctionList[i].Name) - 1));
        end;
      end;
    end;
    idx := FFileData.FileIndex[(Sender as TEditorFrame).FileName];
    if idx = -1 then
      idx := FFileData.CreateFile((Sender as TEditorFrame).FileName);
    FFileData[idx].Functions := (Sender as TEditorFrame).FunctionList;
    FFileData[idx].RequiredFiles := (Sender as TEditorFrame).RequiredFiles;
    FFileData[idx].Variables := (Sender as TEditorFrame).VariableList;
    sl := TStringList.Create;
    try
      for i := 0 to FFileData[idx].RequiredFiles.Count - 1 do
        AddReq(FFileData[idx].RequiredFiles[i], sl);
    finally
      sl.Free
    end;
  end;
end;

procedure TMainForm.ChangeMainForm(FileName: string; Silent: boolean = False);
var
  sl: TStringList;
  e: TEditorFrame;
  f: TFormEditFrame;
  i: integer;
begin
  if FilenameIsAbsolute(FileName) then
    FileName := FCurrentProject.GetRelPath(FileName);
  e := EditorManager1.TextEditor[FCurrentProject.MainFile];
  if Assigned(e) then
  begin
    for i := 0 to e.CodeEditor.Lines.Count - 1 do
      if isEnd(e.CodeEditor.Lines[i], '#include') then
        if Pos('"', e.CodeEditor.Lines[i]) > 0 then
        begin
          if ExtractBetween(e.CodeEditor.Lines[i], '"', '"') =
            ChangeFileExt(FCurrentProject.MainForm, '.au3') then
            e.CodeEditor.TextBetweenPoints[Point(1, i + 1),
              Point(Length(e.CodeEditor.Lines[i]) + 1, i + 1)] :=
              Format('#include<%s>', [ChangeFileExt(FileName,'.au3')]);
        end
        else if ExtractBetween(e.CodeEditor.Lines[i], '<', '>') =
          ChangeFileExt(FCurrentProject.MainForm, '.au3') then
          e.CodeEditor.TextBetweenPoints[Point(1, i + 1),
            Point(Length(e.CodeEditor.Lines[i]) + 1, i + 1)] :=
            Format('#include<%s>', [ChangeFileExt(FileName,'.au3')]);
  end
  else if FileExists(FCurrentProject.MainFile) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(FCurrentProject.MainFile);
      for i := 0 to sl.Count - 1 do
        if isEnd(sl[i], '#include') then
          if pos('"', sl[i]) > 0 then
          begin
            if ExtractBetween(sl[i], '"', '"') = ChangeFileExt(
              FCurrentProject.MainForm, '.au3') then
              sl[i] := Format('#include<%s>', [ChangeFileExt(FileName,'.au3')]);
          end
          else if ExtractBetween(sl[i], '<', '>') = ChangeFileExt(
            FCurrentProject.MainForm, '.au3') then
            sl[i] := Format('#include<%s>', [ChangeFileExt(FileName,'.au3')]);
    finally
      sl.Free;
    end;
  end;
  // Change exit
  f := EditorManager1.FormEditor[GetFullPath(FCurrentProject.MainForm,
    IncludePath, FCurrentProject.ProjectDir, FCurrentProject.Paths)];
  if Assigned(f) then
    f.SetMainForm(False, Silent)
  else if FileExistsUTF8(GetFullPath(FCurrentProject.MainForm,
    IncludePath, FCurrentProject.ProjectDir, FCurrentProject.Paths)) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(GetFullPath(FCurrentProject.MainForm, IncludePath,
        FCurrentProject.ProjectDir, FCurrentProject.Paths));
      for i := 0 to sl.Count - 1 do
        if isEnd(sl[i], 'global') then
        begin
          sl.Delete(i + 3);
          sl.Delete(i);
          Break;
        end;
      sl.SaveToFile(GetFullPath(FCurrentProject.MainForm, IncludePath,
        FCurrentProject.ProjectDir, FCurrentProject.Paths));
    finally
      sl.Free;
    end;
  end;
  f := EditorManager1.FormEditor[GetFullPath(FileName, IncludePath,
    FCurrentProject.ProjectDir, FCurrentProject.Paths)];
  if Assigned(f) then
    f.SetMainForm(True, Silent)
  else if FileExistsUTF8(GetFullPath(FileName, IncludePath,
    FCurrentProject.ProjectDir, FCurrentProject.Paths)) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(GetFullPath(FileName, IncludePath,
        FCurrentProject.ProjectDir, FCurrentProject.Paths));
      for i := 0 to sl.Count - 1 do
        if isEnd(sl[i], 'func') then
          sl.Insert(i, 'Global $PerformClose=True')
        else if isEnd(sl[i], 'endfunc') then
        begin
          sl.Insert(i, '  If ($PerformClose = True) Then Exit');
          Break;
        end;
      sl.SaveToFile(GetFullPath(FileName, IncludePath, FCurrentProject.ProjectDir,
        FCurrentProject.Paths));
    finally
      sl.Free;
    end;
  end;
  FCurrentProject.MainForm := FileName;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
  f: file of TIDEState;
begin
  if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'wnd.cnf') then
  begin
    AssignFile(f, IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'wnd.cnf');
    try
      Reset(f);
      Read(f, FCurrentState);
    finally
      CloseFile(f);
    end;
  end
  else
    with FCurrentState do
    begin
      Width := 800;
      Height := 600;
      State := wsMaximized;
      Top := 200;
      Left := 200;
      PILeft := True;
    end;
  Width := FCurrentState.Width;
  Height := FCurrentState.Height;
  Left := FCurrentState.Left;
  Top := FCurrentState.Top;
  WindowState := FCurrentState.State;
  IDEOptionItem.Checked := FCurrentState.PILeft;
  IDEOptionItemClick(IDEOptionItem);
  FSaveOnClosing := False;
  FFirstLoad := True;
  FCompiler := Tau3Compiler.Create;
  FCompiler.OnOutput := @PrintText;
  FCompiler.OnFinishedCompiling := @FinishedComp;
  FCompiler.OnFinishedRunning := @FinishedRun;
  FCompiler.OnCompileError := @CompileError;
  FFormIsClosing := False;
  FCurrentProject := Tau3Project.Create;
  EditorManager1.EnterFunc := @EnterFunction;
  FFileData := Tau3FileManager.Create;
  EditorManager1.OnParserFinished := @EditorParserFinished;
  EditorManager1.IDEOpenFile := @OpenFile;
  ProjectInspector1.ChangeMainForm := @ChangeMainForm;
  FLastOpend := TStringList.Create;
  FLastOpend.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'LastOpend.txt');
  i := 0;
  while i < FLastOpend.Count do
    if FileExists(FLastOpend[i]) then
      Inc(i)
    else
      FLastOpend.Delete(i);
  Application.ShowMainForm := False;
  Application.QueueAsyncCall(@ShowStartupScreen, 0);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FLastOpend.Free;
  FFileData.Free;
  FCurrentProject.Free;
  FCompiler.Free;
end;

procedure TMainForm.FormOptionsItemClick(Sender: TObject);
begin
  ShowFormConf;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if (WindowState = wsNormal) and (Width <> Screen.Width) then
  begin
    FCurrentState.Width := Width;
    FCurrentState.Height := Height;
    FCurrentState.Left := Left;
    FCurrentState.Top := Top;
  end;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  FCurrentState.State := WindowState;
  if WindowState = wsNormal then
  begin
    Width := FCurrentState.Width;
    Height := FCurrentState.Height;
    Left := FCurrentState.Left;
    Top := FCurrentState.Top;
  end;
end;

procedure TMainForm.IDEOptionItemClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Checked then
  begin
    (Sender as TMenuItem).Caption := 'Projektinspektor Links';
    ProjectInspector1.Align := alLeft;
    ProjectExplorerSplitter.Align := alLeft;
  end
  else
  begin
    (Sender as TMenuItem).Caption := 'Projektinspektor Rechts';
    ProjectInspector1.Align := alRight;
    ProjectExplorerSplitter.Align := alRight;
  end;
  FCurrentState.PILeft := (Sender as TMenuItem).Checked;
end;

procedure TMainForm.InfoMenuItemClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.NewFileItemClick(Sender: TObject);
var
  i: integer;
  s: string;
begin
  i := 1;
  while (EditorManager1.Editor[IncludeTrailingPathDelimiter(
      FCurrentProject.ProjectDir) + 'au3Unit' + IntToStr(i) + '.au3'] >= 0) or
    (FileExists(IncludeTrailingPathDelimiter(FCurrentProject.ProjectDir) +
      'au3Unit' + IntToStr(i) + '.au3')) do
    Inc(i);
  s := IncludeTrailingPathDelimiter(FCurrentProject.ProjectDir) +
    'au3Unit' + IntToStr(i) + '.au3';
  EditorManager1.OpenEditor(s, Point(0, 0)).Parent.Caption := '*' + ExtractFileName(s);
  FCurrentProject.AddFile(s);
end;

procedure TMainForm.NewFormItemClick(Sender: TObject);
var
  fName: string;
  sl: TStringList;
  i: integer;
begin
  i := 1;
  while FileExists(FCurrentProject.GetAbsPath('Form' + IntToStr(i) + '.afm')) do
    Inc(i);
  fName := FCurrentProject.GetAbsPath('Form' + IntToStr(i));
  sl := TStringList.Create;
  try
    sl.Text := '#include<' + 'Form' + IntToStr(i) + '.afm>';
    sl.SaveToFile(fName + '.au3');
    sl.Clear;
    sl.Add('Opt("GUIOnEventMode", 1)');
    sl.Add('Opt("GUIResizeMode", 0)');
    sl.Add(Format('$%s = GUICreate("%s", 402, 344, 200, 200, -1798701056, 256)',
      ['Form' + IntToStr(i), 'Form' + IntToStr(i)]));
    sl.Add(Format('GUISetCursor(2, 0, $%s)', ['Form' + IntToStr(i)]));
    sl.Add('GUISetFont(0,400,0,"default")');
    sl.Add(Format('GUISetBkColor(0xF0F0F0, $%s)', ['Form' + IntToStr(i)]));
    sl.Add('GUISetState(@SW_SHOW)');
    sl.Add(Format('GUISetOnEvent(-3, "%sClose_Exit", $%s)',
      ['Form' + IntToStr(i), 'Form' + IntToStr(i)]));
    sl.Add(Format('Func %sClose_Exit()', ['Form' + IntToStr(i)]));
    sl.Add('EndFunc');
    sl.SaveToFile(fName + '.afm');
  finally
    sl.Free;
  end;
  FCurrentProject.AddFile(fName + '.au3');
  FCurrentProject.AddFile(fName + '.afm');
  EditorManager1.OpenEditor(fName + '.au3', Point(0, 0));
  EditorManager1.OpenEditor(fName + '.afm', Point(0, 0));
end;

procedure TMainForm.NewProjectItemClick(Sender: TObject);
var
  c: TCloseAction;
begin
  c := caNone;
  FormClose(Self, c);
  if EditorManager1.Count > 0 then
    exit;
  FCurrentProject.Clear;
  Application.QueueAsyncCall(@ShowStartupScreen, 0);
end;

procedure TMainForm.NextTabItemClick(Sender: TObject);
begin
  EditorManager1.EditorIndex :=
    Min(EditorManager1.EditorIndex + 1, EditorManager1.Count - 1);
end;

procedure TMainForm.PrevTabItemClick(Sender: TObject);
begin
  EditorManager1.EditorIndex := Max(EditorManager1.EditorIndex - 1, 0);
end;

procedure TMainForm.RedoMenuItemClick(Sender: TObject);
begin
  { TODO : Use Interface instead of this bullshit }
  if EditorManager1.CurrentEditor is TFormEditFrame then
    (EditorManager1.CurrentEditor as TFormEditFrame).DoRedo;
end;

procedure TMainForm.RunBtnClick(Sender: TObject);
begin
  RunMenuItemClick(nil);
end;

procedure TMainForm.RunMenuItemClick(Sender: TObject);
begin
  SaveAllItemClick(nil);
  RunBtn.Enabled := False;
  StopBtn.Enabled := True;
  OutputBox.Clear;
  if SelectModeBox.ItemIndex = 0 then
    FCompiler.Run(FCurrentProject, cax86)
  else
    FCompiler.Run(FCurrentProject, ca64);
end;

procedure TMainForm.SampeButtonClick(Sender: TObject);
begin
  SampleForm.ShowModal;
  if FileExists(SampleForm.Selected) then
    OpenProject(SampleForm.Selected);
end;

procedure TMainForm.SaveAllItemClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to EditorManager1.Count - 1 do
    EditorManager1.EditorSave(i);
  FCurrentProject.Save;
end;

procedure TMainForm.SaveAsItemClick(Sender: TObject);
var
  ext, oldFile, newfile: string;
  i: integer;
begin
  oldFile := EditorManager1.EditorFiles[EditorManager1.EditorIndex];
  ext := ExtractFileExt(oldFile);
  if ext = '.afm' then
    Saveau3FileDialog.Filter := 'AutoIt Formular|*.afm'
  else if ext = '.au3' then
    Saveau3FileDialog.Filter := 'AutoIt Quellcode Datei|*.au3'
  else if ext = '.apr' then
    Saveau3FileDialog.Filter := 'AutoIt Hauptdatei|*.apr';
  Saveau3FileDialog.FileName := ExtractFileName(oldFile);
  if Saveau3FileDialog.Execute then
  begin
    newfile := Saveau3FileDialog.FileName;
    if FileExists(oldFile) then
      DeleteFile(oldFile);
    FFileData.UnloadFile(FFileData.FileIndex[EditorManager1.EditorFiles[
      EditorManager1.EditorIndex]]);
    EditorManager1.EditorSave(EditorManager1.EditorIndex, Saveau3FileDialog.FileName);
    for i := 0 to FCurrentProject.Files.Count - 1 do
      if FCurrentProject.FilePath[i] = oldFile then
      begin
        FCurrentProject.FilePath[i] := Saveau3FileDialog.FileName;
        Break;
      end;
    if (oldFile = FCurrentProject.MainFile) then
      FCurrentProject.MainFile := Saveau3FileDialog.FileName;
    if ext = '.au3' then
    begin
      oldFile := ChangeFileExt(oldFile, '.afm');
      if FileExists(oldFile) then
      begin
        // Add New Include
        AddInclude(Saveau3FileDialog.FileName,
          ChangeFileExt(Saveau3FileDialog.FileName, '.afm'));
        // Change Filename for new Form
        Saveau3FileDialog.FileName := ChangeFileExt(Saveau3FileDialog.FileName, '.afm');
        // Save form file with new name
        if EditorManager1.Editor[oldFile] >= 0 then
          EditorManager1.EditorSave(EditorManager1.Editor[oldFile],
            Saveau3FileDialog.FileName)
        else
        begin
          CopyFile(oldFile, Saveau3FileDialog.FileName);
        end;
        DeleteFile(oldFile);
        // Delete old Include
        CheckInclude(newfile, oldFile);
        if FCurrentProject.GetRelPath(oldFile) = FCurrentProject.MainForm then
        begin
          CheckInclude(FCurrentProject.MainFile, '');
          AddInclude(FCurrentProject.MainFile, newfile);
          FCurrentProject.MainForm :=
            FCurrentProject.GetRelPath(Saveau3FileDialog.FileName);
        end;
        // Change file in Project
        for i := 0 to FCurrentProject.Files.Count - 1 do
          if FCurrentProject.FilePath[i] = oldFile then
          begin
            FCurrentProject.FilePath[i] := Saveau3FileDialog.FileName;
            Break;
          end;
      end;
    end
    else if ext = '.afm' then
    begin
      oldFile := ChangeFileExt(oldFile, '.au3');
      if FileExists(oldFile) then
      begin
        // Change Filename for new Form
        Saveau3FileDialog.FileName := ChangeFileExt(Saveau3FileDialog.FileName, '.au3');
        // Save form file with new name
        if EditorManager1.Editor[oldFile] >= 0 then
          EditorManager1.EditorSave(EditorManager1.Editor[oldFile],
            Saveau3FileDialog.FileName)
        else
        begin
          CopyFile(oldFile, Saveau3FileDialog.FileName);
        end;
        DeleteFile(oldFile);
        // Add New Include
        AddInclude(Saveau3FileDialog.FileName,
          ChangeFileExt(Saveau3FileDialog.FileName, '.afm'));
        // Delete old Include
        CheckInclude(Saveau3FileDialog.FileName, oldFile);
        if FCurrentProject.GetRelPath(ChangeFileExt(oldFile, '.afm')) =
          FCurrentProject.MainForm then
        begin
          CheckInclude(FCurrentProject.MainFile, '');
          AddInclude(FCurrentProject.MainFile, Saveau3FileDialog.FileName);
          FCurrentProject.MainForm :=
            FCurrentProject.GetRelPath(newfile);
        end;
        // Change file in Project
        for i := 0 to FCurrentProject.Files.Count - 1 do
          if FCurrentProject.FilePath[i] = oldFile then
          begin
            FCurrentProject.FilePath[i] := Saveau3FileDialog.FileName;
            Break;
          end;
      end;
    end
    else if ext = '.apr' then
    begin
      DeleteFile(FCurrentProject.ProjectDir + FCurrentProject.Name + '.au3proj');
      FCurrentProject.WriteToFile(FCurrentProject.ProjectDir +
        ChangeFileExt(ExtractFileName(Saveau3FileDialog.FileName), '.au3proj'));
      FLastOpend.Insert(0, FCurrentProject.ProjectDir + FCurrentProject.Name +
        '.au3proj');
    end;
    FFileData.LoadFile(Saveau3FileDialog.FileName);
  end;
end;

procedure TMainForm.SaveFileItemClick(Sender: TObject);
begin
  if FileExists(EditorManager1.EditorFiles[EditorManager1.EditorIndex]) then
    EditorManager1.EditorSave(EditorManager1.EditorIndex)
  else
    SaveAsItemClick(Sender);
end;

end.
