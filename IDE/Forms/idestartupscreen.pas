unit IDEStartupScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, ComCtrls, EditBtn, Project, strutils, au3Types;

type

  { TStartupScreen }

  TStartupScreen = class(TForm)
    Button1: TButton;
    CreateBTN: TButton;
    ClosePanelButton: TButton;
    ImageList1: TImageList;
    NewProjectDirEdit: TDirectoryEdit;
    NewProjectNameEdit: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LastOpendBox: TListBox;
    NewProjectView: TListView;
    NewButton: TSpeedButton;
    OpenButton: TSpeedButton;
    LastOpendPanel: TPanel;
    OpenProjDialog: TOpenDialog;
    NewProjectPanel: TPanel;
    procedure ClosePanelButtonClick(Sender: TObject);
    procedure CreateBTNClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LastOpendBoxDblClick(Sender: TObject);
    procedure LastOpendPanelPaint(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure NewProjectNameEditChange(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
  private
    FPath: string;
    FLastOpend: TStringList;
    procedure SetLastOpend(s: TStringList);
    { private declarations }
  public
    property LastOpend: TStringList read FLastOpend write SetLastOpend;
    property SelectedPath: string read FPath write FPath;
    { public declarations }
  end;

var
  StartupScreen: TStartupScreen;

implementation

{$R *.lfm}

{ TStartupScreen }

procedure TStartupScreen.LastOpendPanelPaint(Sender: TObject);
begin
  (Sender as TCustomControl).Canvas.Pen.Color := clHighlight;
  (Sender as TCustomControl).Canvas.Brush.Style := bsClear;
  (Sender as TCustomControl).Canvas.Pen.Style := psSolid;
  (Sender as TCustomControl).Canvas.Rectangle(0, 0, (Sender as TControl).Width,
    (Sender as TControl).Height);
end;

procedure TStartupScreen.NewButtonClick(Sender: TObject);
begin
  NewProjectPanel.Visible := True;
  LastOpendPanel.Visible := False;
end;

procedure TStartupScreen.NewProjectNameEditChange(Sender: TObject);
begin
  NewProjectDirEdit.Directory :=
    ExtractFilePath(ExcludeTrailingPathDelimiter(NewProjectDirEdit.Directory)) +
    NewProjectNameEdit.Text;
end;

procedure TStartupScreen.OpenButtonClick(Sender: TObject);
begin
  if OpenProjDialog.Execute then
  begin
    ModalResult := mrYes;
    FPath := OpenProjDialog.FileName;
    Close;
  end;
end;

procedure TStartupScreen.SetLastOpend(s: TStringList);
var
  i: integer;
begin
  FLastOpend.Assign(s);
  LastOpendBox.Clear;
  for i := 0 to FLastOpend.Count - 1 do
    LastOpendBox.Items.Add(ExtractFileName(ExtractFileNameWithoutExt(FLastOpend[i])));
end;

procedure TStartupScreen.LastOpendBoxDblClick(Sender: TObject);
begin
  if LastOpendBox.ItemIndex >= 0 then
  begin
    ModalResult := mrYes;
    FPath := FLastOpend[LastOpendBox.ItemIndex];
    Close;
  end;
end;

procedure TStartupScreen.FormCreate(Sender: TObject);
begin
  FLastOpend := TStringList.Create;
end;

procedure TStartupScreen.ClosePanelButtonClick(Sender: TObject);
begin
  NewProjectPanel.Hide;
  LastOpendPanel.Show;
end;

procedure TStartupScreen.CreateBTNClick(Sender: TObject);

  function IsValid(str: string): boolean;
  var
    i: integer;
  begin
    Result := True;
    for i := 1 to Length(str) do
      if not (str[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      begin
        Result := False;
        Break;
      end;
  end;

var
  p: Tau3Project;
  sl: TStringList;
  path, preset, mf: string;
  i: integer;
begin
  if NewProjectView.ItemIndex < 0 then
  begin
    ShowMessage('Bitte einen Projekttypen auswählen');
    Exit;
  end;
  if not isValid(NewProjectNameEdit.Text) then
  begin
    ShowMessage('Der Name darf nur aus Alphanumerischen Zeichen und _ bestehen');
    Exit;
  end;
  if not DirectoryExists(NewProjectDirEdit.Directory) then
  begin
    if not ForceDirectories(NewProjectDirEdit.Directory) then
    begin
      ShowMessage('Fehler beim erstellen des Projektverzeichnisses');
      exit;
    end;
  end
  else
  begin
    sl := TStringList.Create;
    try
      FindAllFiles(sl, NewProjectDirEdit.Directory);
      if (sl.Count > 0) and (MessageDlg('Verzeichnis ist nicht leer',
        'Das gewählte Projektverzeichnis ist nicht leer, wollen sie dennoch ' +
        'fortfahren?'#10#13'Möglicherweise werden bereits vorhandend Dateien überschrieben',
        mtWarning, mbYesNo, 'Dateien überschreiben') = mrNo) then
        Exit;
    finally
      sl.Free;
    end;
  end;
  path := IncludeTrailingPathDelimiter(NewProjectDirEdit.Directory);
  preset := ExtractFilePath(ParamStr(0)) + 'Presets' + PathDelim +
    NewProjectView.Selected.Caption + PathDelim;
  p := Tau3Project.Create;
  try
    p.ProjectDir := path;
    p.Name := NewProjectNameEdit.Text;
    p.Files.LoadFromFile(preset + 'Filelist.txt');
    sl := TStringList.Create;
    try
      sl.LoadFromFile(preset + 'PresetInfo.txt');
      for i := 0 to sl.Count - 1 do
        if AnsiStartsStr('MainFile=', sl[i]) then
          mf := Trim(Copy(sl[i], pos('=', sl[i]) + 1, length(sl[i]) - pos('=', sl[i])))
        else if AnsiStartsStr('FocusedFile=', sl[i]) then
          p.FocusedFile := StrToInt(Trim(Copy(sl[i], pos('=', sl[i]) + 1,
            length(sl[i]) - pos('=', sl[i]))))
        else if AnsiStartsStr('OpendFile=', sl[i]) then
          p.OpendFiles.Add(OpendFileInfo(Trim(Copy(sl[i], pos('=', sl[i]) + 1,
            length(sl[i]) - pos('=', sl[i])))))
        else if AnsiStartsStr('AppType=', sl[i]) then
          p.GUIBased := Trim(Copy(sl[i], pos('=', sl[i]) + 1,
            length(sl[i]) - pos('=', sl[i]))) = 'GUI'
        else if AnsiStartsStr('MainForm=', sl[i]) then
          p.MainForm:=Trim(Copy(sl[i], pos('=', sl[i]) + 1,
            length(sl[i]) - pos('=', sl[i])));
    finally
      sl.Free;
    end;
    for i := 0 to p.Files.Count - 1 do
    begin
      ForceDirectories(path + ExtractFilePath(p.Files[i]));
      CopyFile(preset + p.Files[i], path + p.Files[i]);
    end;
    CopyFile(preset + mf, path + p.Name + '.apr');
    p.MainFile := path + p.Name + '.apr';
    if p.OpendFiles[p.FocusedFile].Name = mf then
      p.OpendFiles[p.FocusedFile] := OpendFileInfo(p.GetMainFileRel);
    p.Save;
    FPath := path + p.Name + '.au3proj';
  finally
    p.Free;
  end;
  ModalResult:=mrYes;
  ClosePanelButtonClick(Sender);
  Close;
end;

procedure TStartupScreen.FormDestroy(Sender: TObject);
begin
  FLastOpend.Free;
end;

procedure TStartupScreen.FormShow(Sender: TObject);

  procedure GetSubDirs(Dir: string; slt: TStrings);
  var
    srSearch: TSearchRec;
  begin
    slt.BeginUpdate;
    try
      Dir := IncludeTrailingPathDelimiter(Dir);
      if FindFirst(Dir + '*', faDirectory, srSearch) = 0 then
        repeat
          if ((srSearch.Attr and faDirectory) = faDirectory) and
            (srSearch.Name <> '.') and (srSearch.Name <> '..') then
          begin
            slt.Add(srSearch.Name);
          end;
        until (FindNext(srSearch) <> 0);

    finally
      slt.EndUpdate;
      FindClose(srSearch);
    end;
  end;

  procedure LoadPNG(b: TBitmap; f: string);
  var
    p: TPortableNetworkGraphic;
  begin
    p := TPortableNetworkGraphic.Create;
    try
      p.LoadFromFile(f);
      b.Assign(p);
    finally
      p.Free;
    end;
  end;

var
  lst: TStringList;
  i, img: integer;
  b: TBitmap;
begin
  FPath:='';
  NewProjectDirEdit.Directory :=
    GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH') +
    PathDelim + 'au3' + PathDelim + 'au3Projekt1';
  lst := TStringList.Create;
  try
    NewProjectView.Clear;
    while ImageList1.Count > 1 do
      ImageList1.Delete(1);
    GetSubDirs(ExtractFilePath(ParamStr(0)) + 'Presets', lst);
    for i := 0 to lst.Count - 1 do
      with NewProjectView.Items.Add do
      begin
        if FileExists(ExtractFilePath(ParamStr(0)) + 'Presets' +
          PathDelim + lst[i] + PathDelim + 'Icon.png') then
        begin
          b := TBitmap.Create;
          try
            LoadPNG(b, ExtractFilePath(ParamStr(0)) + 'Presets' +
              PathDelim + lst[i] + PathDelim + 'Icon.png');
            img := ImageList1.Add(b, nil);
          finally
            b.Free;
          end;
        end
        else
          img := 0;
        ImageIndex := img;
        Caption := lst[i];
      end;
  finally
    lst.Free;
  end;
end;

end.
