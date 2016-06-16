unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtendedNotebook, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Menus, ComCtrls, Editor;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGDLG: TColorDialog;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    SiteBarDLG: TColorDialog;
    FontDialog1: TFontDialog;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    CloseFileItem: TMenuItem;
    CloseAllItem: TMenuItem;
    MenuItem14: TMenuItem;
    QuitItem: TMenuItem;
    OpenItem: TMenuItem;
    SaveItem: TMenuItem;
    SaveAsItem: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    NewItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    SaveDialog1: TSaveDialog;
    procedure Button2Click(Sender: TObject);
    procedure CloseAllItemClick(Sender: TObject);
    procedure CloseFileItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure NewItemClick(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
  private
    procedure PageChanged(Sender: TObject);
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.PageChanged(Sender: TObject);
begin
  if not ((Sender as TEditorFrame).Parent.Caption[1] = '*') then
    (Sender as TEditorFrame).Parent.Caption :=
      '*' + (Sender as TEditorFrame).Parent.Caption;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  //if OpenDialog1.Execute then
  //EditorFrame1.Load(OpenDialog1.FileName);
end;

procedure TForm1.CloseAllItemClick(Sender: TObject);
begin
  while PageControl1.PageCount > 0 do
    CloseFileItemClick(nil);
  NewItemClick(nil);
end;

procedure TForm1.CloseFileItemClick(Sender: TObject);
begin
  if PageControl1.ActivePage.Caption[1] = '*' then
    if MessageDlg('Datei Sichern?', Format('Die Datei %s wurde geändert, sichern?',
      [(PageControl1.ActivePage.Components[0] as TEditorFrame).FileName]),
      mtConfirmation, mbYesNo, 'Save') = mrYes then
      SaveItemClick(nil);
  (PageControl1.ActivePage.Components[0] as TEditorFrame).Free;
  PageControl1.ActivePage.Free;
  if Assigned(Sender) and (PageControl1.PageCount = 0) then
    NewItemClick(nil);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  QuitItemClick(nil);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  tmp: TTabSheet;
begin
  if (Paramcount > 0) then
  begin
    for i := 1 to Paramcount do
      if FileExists(ParamStr(i)) then
      begin
        tmp := PageControl1.AddTabSheet;
        tmp.Caption := 'Neu';
        PageControl1.ActivePage := tmp;
        tmp.Visible := True;
        with TEditorFrame.Create(tmp) do
        begin
          Font := FontDialog1.Font;
          CodeEditor.Color := BGDLG.Color;
          CodeEditor.Gutter.Color := SiteBarDLG.Color;
          Align := alClient;
          Parent := tmp;
          Visible := True;
          OnChange := @PageChanged;
          Load(ParamStr(i));
        end;
      end;
  end
  else
    Application.QueueAsyncCall(TDataEvent(@NewItemClick), 0);
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  (PageControl1.ActivePage.Components[0] as TEditorFrame).StartFormatter;
end;

procedure TForm1.NewItemClick(Sender: TObject);
var
  tmp: TTabSheet;
begin
  tmp := PageControl1.AddTabSheet;
  tmp.Caption := 'Neu';
  PageControl1.ActivePage := tmp;
  tmp.Visible := True;
  with TEditorFrame.Create(tmp) do
  begin
    Font := FontDialog1.Font;
    CodeEditor.Color := BGDLG.Color;
    CodeEditor.Gutter.Color := SiteBarDLG.Color;
    Align := alClient;
    Parent := tmp;
    Visible := True;
    CodeEditor.SetFocus;
    OnChange := @PageChanged;
  end;
end;

procedure TForm1.OpenItemClick(Sender: TObject);
var
  i, x: integer;
  b: Boolean;
begin
  if OpenDialog1.Execute then
  begin
    if FileExists(OpenDialog1.FileName) then
    begin
      for i := 0 to OpenDialog1.Files.Count - 1 do
      begin
        b:=True;
        for x:=0 to PageControl1.PageCount-1 do
          if ((PageControl1.Pages[x].Components[0] as TEditorFrame).FileName='')
          And (Trim((PageControl1.Pages[x].Components[0] as TEditorFrame).CodeEditor.Text)='') then
          begin
            PageControl1.PageIndex:=x;
            b:=False;
            Break;
          end;
        if b then
          NewItemClick(nil);
        (PageControl1.ActivePage.Components[0] as TEditorFrame).Load(
          OpenDialog1.Files[i]);
        PageControl1.ActivePage.Caption :=
          ExtractFileName(OpenDialog1.Files[i]);
      end;
    end;
  end;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage.ComponentCount>0 then
    (PageControl1.ActivePage.Components[0] as TEditorFrame).SetFocus;
end;

procedure TForm1.QuitItemClick(Sender: TObject);
begin
  while PageControl1.PageCount > 0 do
  begin
    if PageControl1.ActivePage.Caption[1] = '*' then
      if MessageDlg('Datei Sichern?', Format('Die Datei %s wurde geändert, sichern?',
        [(PageControl1.ActivePage.Components[0] as TEditorFrame).FileName]),
        mtConfirmation, mbYesNo, 'Save') = mrYes then
        SaveItemClick(nil);
    (PageControl1.ActivePage.Components[0] as TEditorFrame).Free;
    PageControl1.ActivePage.Free;
  end;
  if Assigned(Sender) then
    Close;
end;

procedure TForm1.SaveItemClick(Sender: TObject);
begin
  if (PageControl1.ActivePage.Components[0] as TEditorFrame).FileName = '' then
    SaveAsItemClick(nil)
  else
    (PageControl1.ActivePage.Components[0] as TEditorFrame).Save();
  if (PageControl1.ActivePage.Components[0] as TEditorFrame).FileName <> '' then
    PageControl1.ActivePage.Caption :=
      ExtractFileName((PageControl1.ActivePage.Components[0] as TEditorFrame).FileName);
end;

procedure TForm1.SaveAsItemClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    if FileExists(SaveDialog1.FileName) then
      DeleteFile(SaveDialog1.FileName);
    (PageControl1.ActivePage.Components[0] as TEditorFrame).Save(SaveDialog1.FileName);
    PageControl1.ActivePage.Caption :=
      ExtractFileName((PageControl1.ActivePage.Components[0] as TEditorFrame).FileName);
  end;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  //FontDialog1.Font := EditorFrame1.Font;
  //if FontDialog1.Execute then
  //EditorFrame1.Font := FontDialog1.Font;
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  //BGDLG.Color := EditorFrame1.CodeEditor.Color;
  if BGDLG.Execute then;
  //EditorFrame1.CodeEditor.Color := BGDLG.Color;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  //BGDLG.Color := EditorFrame1.CodeEditor.Gutter.Color;
  //if BGDLG.Execute then
  //EditorFrame1.CodeEditor.Gutter.Color := BGDLG.Color;
end;

end.
