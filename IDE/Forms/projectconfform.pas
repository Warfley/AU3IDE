unit ProjectConfForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, StdCtrls, EditBtn, ValEdit, TLStrings, LCLTranslator;

type

  { TProjectSettings }

  TProjectSettings = class(TForm)
    AddParamBtn: TButton;
    AppTypeBox: TComboBox;
    Label10: TLabel;
    UseVersion: TCheckBox;
    IncBuildBox: TCheckBox;
    VersionEdit: TEdit;
    SubversionEdit: TEdit;
    RevisionEdit: TEdit;
    BuiltEdit: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    TabSheet2: TTabSheet;
    UPXBox: TCheckBox;
    DeleteParamBtn: TButton;
    CancelButton: TButton;
    CompileEdit: TFileNameEdit;
    IconEdit: TFileNameEdit;
    DirEdit: TDirectoryEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    CompLabel: TLabel;
    NameEdit: TEdit;
    ParamEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ParamBox: TListBox;
    Panel2: TPanel;
    SaveButton: TButton;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Allgemein: TTabSheet;
    Compiler: TTabSheet;
    RunParameter: TTabSheet;
    CompTrackBar: TTrackBar;
    VersionData: TValueListEditor;
    procedure AddParamBtnClick(Sender: TObject);
    procedure UseVersionChange(Sender: TObject);
    procedure DeleteParamBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ParamEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CompTrackBarChange(Sender: TObject);
    procedure VersionEditChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ProjectSettings: TProjectSettings;

const
  ACompStr: array[0..4] of string = (SComp0, SComp1, SComp2, SComp3, SComp4);

implementation

{$R *.lfm}

{ TProjectSettings }

procedure TProjectSettings.AddParamBtnClick(Sender: TObject);
begin
  if ParamEdit.Text <> '' then
  begin
    ParamBox.Items.Add(ParamEdit.Text);
    ParamEdit.SelectAll;
    ParamEdit.SetFocus;
  end;
end;

procedure TProjectSettings.UseVersionChange(Sender: TObject);
begin
  VersionEdit.Enabled := UseVersion.Checked;
  SubversionEdit.Enabled := UseVersion.Checked;
  RevisionEdit.Enabled := UseVersion.Checked;
  BuiltEdit.Enabled := UseVersion.Checked;
  IncBuildBox.Enabled := UseVersion.Checked;
  if UseVersion.Checked then
    VersionData.Values['FileVersion'] :=
      Format('%s.%s.%s.%s', [VersionEdit.Text, SubversionEdit.Text,
      RevisionEdit.Text, BuiltEdit.Text]);
end;

procedure TProjectSettings.DeleteParamBtnClick(Sender: TObject);
begin
  if ParamBox.ItemIndex > 0 then
    ParamBox.Items.Delete(ParamBox.ItemIndex);
end;

procedure TProjectSettings.FormShow(Sender: TObject);
begin
  CompTrackBarChange(nil);
  VersionData.Col:=0;
end;

procedure TProjectSettings.ParamEditKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if key = 13 then
    AddParamBtnClick(nil);
end;

procedure TProjectSettings.CompTrackBarChange(Sender: TObject);
begin
  CompLabel.Caption := ACompStr[CompTrackBar.Position];
end;

procedure TProjectSettings.VersionEditChange(Sender: TObject);
begin
  if UseVersion.Checked then
    VersionData.Values['Version'] :=
      Format('%s.%s.%s.%s', [VersionEdit.Text, SubversionEdit.Text,
      RevisionEdit.Text, BuiltEdit.Text]);
end;

end.
