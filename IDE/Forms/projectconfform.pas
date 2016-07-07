unit ProjectConfForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, StdCtrls, EditBtn;

type

  { TProjectSettings }

  TProjectSettings = class(TForm)
    AddParamBtn: TButton;
    UPXBox: TCheckBox;
    DeleteParamBtn: TButton;
    CancelButton: TButton;
    CompileEdit: TFileNameEdit;
    IconEdit: TFileNameEdit;
    GUIBox: TCheckBox;
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
    TabSheet1: TTabSheet;
    CompTrackBar: TTrackBar;
    procedure AddParamBtnClick(Sender: TObject);
    procedure DeleteParamBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ParamEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CompTrackBarChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ProjectSettings: TProjectSettings;

const
  ACompStr: array[0..4] of String = ('Keine', 'Schwach', 'Normal', 'Stark', 'Max');

implementation

{$R *.lfm}

{ TProjectSettings }

procedure TProjectSettings.AddParamBtnClick(Sender: TObject);
begin
  if ParamEdit.Text<>'' then
  begin
    ParamBox.Items.Add(ParamEdit.Text);
    ParamEdit.SelectAll;
    ParamEdit.SetFocus;
  end;
end;

procedure TProjectSettings.DeleteParamBtnClick(Sender: TObject);
begin
  if ParamBox.ItemIndex>0 then
    ParamBox.Items.Delete(ParamBox.ItemIndex);
end;

procedure TProjectSettings.FormShow(Sender: TObject);
begin
  CompTrackBarChange(nil);
end;

procedure TProjectSettings.ParamEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 13 then AddParamBtnClick(nil);
end;

procedure TProjectSettings.CompTrackBarChange(Sender: TObject);
begin
  CompLabel.Caption:=ACompStr[CompTrackBar.Position];
end;

end.

