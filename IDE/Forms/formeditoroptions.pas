unit FormEditorOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, au3Types, TLStrings, LCLTranslator;

type

  { TFormEditorOptionsForm }

  TFormEditorOptionsForm = class(TForm)
    BGColorBtn: TButton;
    HelpLineBox: TCheckBox;
    DBBox: TCheckBox;
    RasterSizeEdit: TEdit;
    Label6: TLabel;
    RasterBox: TCheckBox;
    ForeColorButton: TButton;
    Label5: TLabel;
    TBColorButton: TButton;
    BGColorPicklist: TColorBox;
    ForeColorPicklist: TColorBox;
    TBColorPicklist: TColorBox;
    Button1: TButton;
    Button2: TButton;
    ColorDialog1: TColorDialog;
    Label3: TLabel;
    Label4: TLabel;
    OISideBox: TToggleBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure BGColorBtnClick(Sender: TObject);
    procedure BGColorPicklistChange(Sender: TObject);
    procedure ForeColorButtonClick(Sender: TObject);
    procedure OISideBoxChange(Sender: TObject);
    procedure TBColorButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    procedure Load(P: string);
    procedure Save(P: string);
    { public declarations }
  end;

var
  FormEditorOptionsForm: TFormEditorOptionsForm;

implementation

{$R *.lfm}

{ TFormEditorOptionsForm }

procedure TFormEditorOptionsForm.BGColorBtnClick(Sender: TObject);
begin
  ColorDialog1.Color := BGColorPicklist.Selected;
  if ColorDialog1.Execute then
    BGColorPicklist.Selected := ColorDialog1.Color;

  if BGColorPicklist.Text = '' then
    BGColorPicklistChange(BGColorPicklist);
end;

procedure TFormEditorOptionsForm.BGColorPicklistChange(Sender: TObject);
begin
  if (Sender as TColorBox).Text = '' then
    (Sender as TColorBox).Color := (Sender as TColorBox).Selected
  else
    (Sender as TColorBox).Color := clDefault;
end;

procedure TFormEditorOptionsForm.ForeColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := ForeColorPicklist.Selected;
  if ColorDialog1.Execute then
    ForeColorPicklist.Selected := ColorDialog1.Color;

  if BGColorPicklist.Text = '' then
    BGColorPicklistChange(ForeColorPicklist);
end;

procedure TFormEditorOptionsForm.OISideBoxChange(Sender: TObject);
begin
  if OISideBox.Checked then
    OISideBox.Caption := SRight
  else
    OISideBox.Caption := SLeft;
end;

procedure TFormEditorOptionsForm.TBColorButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := TBColorPicklist.Selected;
  if ColorDialog1.Execute then
    TBColorPicklist.Selected := ColorDialog1.Color;

  if BGColorPicklist.Text = '' then
    BGColorPicklistChange(TBColorPicklist);
end;

procedure TFormEditorOptionsForm.Load(P: string);
var
  conf: TFormEditorConfig;
  f: file of TFormEditorConfig;
begin
  AssignFile(f, P);
  try
    Reset(f);
    Read(f, conf);
  finally
    CloseFile(f);
  end;
  with conf do
  begin
    OISideBox.Checked := OIRight;
    BGColorPicklist.Selected := BGCol;
    ForeColorPicklist.Selected := ForeCol;
    TBColorPicklist.Selected := TBCol;
    RasterBox.Checked:=UseRaster;
    RasterSizeEdit.Text:=IntToStr(RasterSize);
    HelpLineBox.Checked:=UseHelpLines;
    DBBox.Checked:=DoubleBuffer;
  end;
end;

procedure TFormEditorOptionsForm.Save(P: string);
var
  conf: TFormEditorConfig;
  f: file of TFormEditorConfig;
begin
  with conf do
  begin
    OIRight := OISideBox.Checked;
    BGCol := ColorToRGB(BGColorPicklist.Selected);
    ForeCol := ColorToRGB(ForeColorPicklist.Selected);
    TBCol := ColorToRGB(TBColorPicklist.Selected);
    UseRaster:=RasterBox.Checked;
    RasterSize:=StrToInt(RasterSizeEdit.Text);
    UseHelpLines:=HelpLineBox.Checked;
    DoubleBuffer:=DBBox.Checked;
  end;
  AssignFile(f, P);
  try
    Rewrite(f);
    Write(f, conf);
  finally
    CloseFile(f);
  end;
end;

end.
