unit ProjectInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, ComCtrls,
  ExtCtrls, StdCtrls, Project, Dialogs, au3Types, ProjectConfForm, TLStrings;

type

  { TProjectInspector }

  TProjectInspector = class(TFrame)
    AddButton: TButton;
    SettingsButton: TButton;
    Openau3FileDialog: TOpenDialog;
    SetMainFormButton: TButton;
    RenameButton: TButton;
    DeleteButton: TButton;
    FileIconList: TImageList;
    FileInfoPanel: TPanel;
    Label1: TLabel;
    PathEdit: TLabeledEdit;
    ProjectFileTreeView: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure AddButtonClick(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure ProjectFileTreeViewClick(Sender: TObject);
    procedure ProjectFileTreeViewDblClick(Sender: TObject);
    procedure SetMainFormButtonClick(Sender: TObject);
  private
    FMainForm: TChangeMainFormEvent;
    FProject: Tau3Project;
    FOpenEditor: TOpenEditorEvent;
    FCloseEditor: TCloseEditorEvent;
    procedure SetProject(p: Tau3Project);
    procedure ProjChanged(Sender: TObject);
    { private declarations }
  public
    property ChangeMainForm: TChangeMainFormEvent read FMainForm write FMainForm;
    property Project: Tau3Project read FProject write SetProject;
    property OpenEditor: TOpenEditorEvent read FOpenEditor write FOpenEditor;
    property CloseEditor: TCloseEditorEvent read FCloseEditor write FCloseEditor;
    { public declarations }
  end;

implementation

{$R *.lfm}

procedure TProjectInspector.ProjectFileTreeViewClick(Sender: TObject);
begin
  DeleteButton.Enabled := False;
  if Assigned(ProjectFileTreeView.Selected) then
  begin
    PathEdit.Visible := True;
    if ProjectFileTreeView.Selected.Data = Pointer(-1) then
      PathEdit.Text := FProject.MainFile
    else if ProjectFileTreeView.Selected.Data = Pointer(-2) then
      PathEdit.Text := FProject.ProjectDir
    else if IntPtr(ProjectFileTreeView.Selected.Data) >= 0 then
    begin
      DeleteButton.Enabled := True;
      PathEdit.Text := FProject.FilePath[IntPtr(ProjectFileTreeView.Selected.Data)];
    end
    else
      PathEdit.Visible := True;
    SetMainFormButton.Visible :=
      (ExtractFileExt(ProjectFileTreeView.Selected.Text) = '.afm') and (FProject.AppType=atGUI);
    SetMainFormButton.Enabled :=
      (IntPtr(ProjectFileTreeView.Selected.Data) >= 0) and not
      (FProject.MainForm = FProject.Files[IntPtr(ProjectFileTreeView.Selected.Data)]);
  end;

end;

procedure TProjectInspector.FrameResize(Sender: TObject);
begin
  FileInfoPanel.Height := (ClientHeight - Label1.Height) div 3;
end;

procedure TProjectInspector.AddButtonClick(Sender: TObject);
var
  i: integer;
  hasForm: boolean;
begin
  if Openau3FileDialog.Execute then
  begin
    hasForm := (ExtractFileExt(Openau3FileDialog.FileName) = '.au3') and
      FileExists(ChangeFileExt(Openau3FileDialog.FileName, '.afm'));
    if Pos(FProject.ProjectDir, Openau3FileDialog.FileName) < 1 then
      if MessageDlg(SFileNotProjDirTitle, SFileNotProjDirText,
        mtConfirmation, mbYesNo, SFileNotProjDirKeyword) = mrYes then
      begin
        if hasForm then
          CopyFile(ChangeFileExt(Openau3FileDialog.FileName, '.afm'),
            IncludeTrailingPathDelimiter(FProject.ProjectDir) +
            ChangeFileExt(ExtractFileName(Openau3FileDialog.FileName), '.afm'));
        CopyFile(Openau3FileDialog.FileName,
          IncludeTrailingPathDelimiter(FProject.ProjectDir) +
          ExtractFileName(Openau3FileDialog.FileName));
        Openau3FileDialog.FileName :=
          IncludeTrailingPathDelimiter(FProject.ProjectDir) +
          ExtractFileName(Openau3FileDialog.FileName);
      end;
    if Assigned(FOpenEditor) then
      FOpenEditor(Openau3FileDialog.FileName, Point(0, 0));
    for i := 0 to FProject.Files.Count - 1 do
      if FProject.FilePath[i] = Openau3FileDialog.FileName then
        exit;
    FProject.AddFile(Openau3FileDialog.FileName);
    if hasForm then
    begin
      Openau3FileDialog.FileName := ChangeFileExt(Openau3FileDialog.FileName, '.afm');
      if Assigned(FOpenEditor) then
        FOpenEditor(Openau3FileDialog.FileName, Point(0, 0));
      for i := 0 to FProject.Files.Count - 1 do
        if FProject.FilePath[i] = Openau3FileDialog.FileName then
          exit;
      FProject.AddFile(Openau3FileDialog.FileName);
    end;
  end;
end;

procedure TProjectInspector.SettingsButtonClick(Sender: TObject);
var
  co: TCompOptions;
  i: integer;
  v: TVersion;
begin
  ProjectSettings.NameEdit.Text := FProject.Name;
  ProjectSettings.DirEdit.Directory := FProject.ProjectDir;
  ProjectSettings.AppTypeBox.ItemIndex := ord(FProject.AppType);
  ProjectSettings.CompileEdit.FileName :=
    CreateAbsolutePath(FProject.CompilerOptions.OutPath, FProject.ProjectDir);
  ProjectSettings.IconEdit.FileName := FProject.CompilerOptions.IconPath;
  ProjectSettings.CompTrackBar.Position := Ord(FProject.CompilerOptions.Compression);
  ProjectSettings.UPXBox.Checked := FProject.CompilerOptions.PackUPX;
  ProjectSettings.ParamBox.Items.Assign(FProject.RunParams);
  v := FProject.Version;
  with v do
  begin
    ProjectSettings.UseVersion.Checked := UseVersion;
    ProjectSettings.IncBuildBox.Checked := IncreaseBuilt;
    ProjectSettings.VersionEdit.Text := IntToStr(Version);
    ProjectSettings.SubversionEdit.Text := IntToStr(Subversion);
    ProjectSettings.RevisionEdit.Text := IntToStr(Revision);
    ProjectSettings.BuiltEdit.Text := IntToStr(Built);
  end;

  ProjectSettings.VersionData.Clear;
  for i := 0 to FProject.VersionData.Count - 1 do
    ProjectSettings.VersionData.Strings.Values[FProject.VersionData.Names[i]]:= FProject.VersionData.ValueFromIndex[i];
  if ProjectSettings.ShowModal = mrOk then
  begin
    FProject.WriteToFile(IncludeTrailingPathDelimiter(
      ProjectSettings.DirEdit.Directory) + ProjectSettings.NameEdit.Text +
      '.au3proj');
    FProject.AppType := TAppType(ProjectSettings.AppTypeBox.ItemIndex);
    with co do
    begin
      if CreateRelativePath(ProjectSettings.CompileEdit.FileName,
        FProject.ProjectDir, True) <> FProject.CompilerOptions.OutPath then
        OutPath := CreateRelativePath(ProjectSettings.CompileEdit.FileName,
          FProject.ProjectDir, True);
      IconPath := ProjectSettings.IconEdit.FileName;
      Compression := TCompressionMode(ProjectSettings.CompTrackBar.Position);
      PackUPX := ProjectSettings.UPXBox.Checked;
    end;
    FProject.CompilerOptions := co;
    with v do
    begin
      UseVersion := ProjectSettings.UseVersion.Checked;
      IncreaseBuilt := ProjectSettings.IncBuildBox.Checked;
      Version := StrToInt(ProjectSettings.VersionEdit.Text);
      Subversion := StrToInt(ProjectSettings.SubversionEdit.Text);
      Revision := StrToInt(ProjectSettings.RevisionEdit.Text);
      Built := StrToInt(ProjectSettings.BuiltEdit.Text);
    end;
    FProject.Version := v;
  FProject.VersionData.Clear;
  for i := 0 to ProjectSettings.VersionData.Strings.Count - 1 do
    FProject.VersionData.Values[ProjectSettings.VersionData.Strings.Names[i]]:= ProjectSettings.VersionData.Strings.ValueFromIndex[i];
    FProject.RunParams.Assign(ProjectSettings.ParamBox.Items);
    FProject.Save;
  end;
end;

procedure TProjectInspector.DeleteButtonClick(Sender: TObject);
var
  fm, p: string;
begin
  if MessageDlg(SDeleteFileTitle, SDeleteFileText,
    mtConfirmation, mbYesNo, SDeleteFileKeyword) = mrYes then
  begin
    fm := '';
    p := FProject.FilePath[IntPtr(ProjectFileTreeView.Selected.Data)];
    if ExtractFileExt(p) = '.au3' then
      fm := ChangeFileExt(p, '.afm')
    else if ExtractFileExt(p) = '.afm' then
      fm := ChangeFileExt(p, '.au3');
    if FileExists(fm) then
    begin
      DeleteFile(fm);
      if Assigned(FCloseEditor) then
        FCloseEditor(fm);
      FProject.DeleteFile(fm);
    end;
    DeleteFile(p);
    if Assigned(FCloseEditor) then
      FCloseEditor(p);
    FProject.DeleteFile(p);
  end;
end;

procedure TProjectInspector.ProjectFileTreeViewDblClick(Sender: TObject);
begin
  if Assigned(ProjectFileTreeView.Selected) and Assigned(FOpenEditor) then
    if ProjectFileTreeView.Selected.Data = Pointer(-1) then
      FOpenEditor(FProject.MainFile, Point(0, 0))
    else if IntPtr(ProjectFileTreeView.Selected.Data) >= 0 then
      FOpenEditor(FProject.FilePath[IntPtr(ProjectFileTreeView.Selected.Data)],
        Point(0, 0));
end;

procedure TProjectInspector.SetMainFormButtonClick(Sender: TObject);
begin
  if Assigned(ProjectFileTreeView.Selected) and
    (ExtractFileExt(ProjectFileTreeView.Selected.Text) = '.afm') then
  begin
    if Assigned(FMainForm) then
      FMainForm(FProject.FilePath[IntPtr(ProjectFileTreeView.Selected.Data)]);
    SetMainFormButton.Enabled := False;
  end;
end;

procedure TProjectInspector.SetProject(p: Tau3Project);
begin
  FProject := p;
  FProject.OnChange := @ProjChanged;
  ProjChanged(FProject);
end;

procedure TProjectInspector.ProjChanged(Sender: TObject);

  function CreateDirNode(p: string): TTreeNode;
  var
    s: string;
    i: integer;
  begin
    s := ExtractFileName(ExcludeTrailingPathDelimiter(p));
    if s = '' then
      Result := ProjectFileTreeView.Items[0]
    else
    begin
      Result := CreateDirNode(ExtractFilePath(ExcludeTrailingPathDelimiter(p)));
      for i := 0 to Result.Count - 1 do
        if Result.Items[i].Text = s then
        begin
          Result := Result.Items[i];
          Exit;
        end;
      Result := ProjectFileTreeView.Items.AddChild(Result, s);
      Result.ImageIndex := 1;
      Result.SelectedIndex := 1;
      Result.Data := Pointer(-3);
    end;
  end;

var
  i: integer;
  s: string;
  p: TTreeNode;
  ext: string;
  tmp: TTreeNode;
begin
  ProjectFileTreeView.Items.Clear;
  tmp := ProjectFileTreeView.Items.Add(nil, FProject.Name);
  tmp.ImageIndex := 4;
  tmp.SelectedIndex := 4;
  tmp.Data := Pointer(-2);
  s := FProject.GetMainFileRel;
  p := CreateDirNode(ExtractFilePath(s));
  tmp := ProjectFileTreeView.Items.AddChild(p, ExtractFileName(s));
  tmp.ImageIndex := 2;
  tmp.SelectedIndex := 2;
  tmp.Data := Pointer(-1);
  for i := 0 to FProject.Files.Count - 1 do
  begin
    s := FProject.Files[i];
    p := CreateDirNode(ExtractFilePath(s));
    ext := ExtractFileExt(s);
    tmp := ProjectFileTreeView.Items.AddChild(p, ExtractFileName(s));
    tmp.Data := Pointer(i);
    if ext = '.au3' then
    begin
      tmp.ImageIndex := 2;
      tmp.SelectedIndex := 2;
    end
    else if ext = '.afm' then
    begin
      tmp.ImageIndex := 3;
      tmp.SelectedIndex := 3;
    end
    else
    begin
      tmp.ImageIndex := 0;
      tmp.SelectedIndex := 0;
    end;
  end;
  ProjectFileTreeView.FullExpand;
end;

end.
