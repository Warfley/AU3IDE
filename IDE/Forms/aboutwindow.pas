unit AboutWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, au3Types, LCLIntf;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Hilfe: TButton;
    Button2: TButton;
    Image1: TImage;
    InfoMemo: TMemo;
    Label1: TLabel;
    Panel1: TPanel;
    procedure HilfeClick(Sender: TObject);
    procedure Image1Paint(Sender: TObject);
    procedure InfoMemoEnter(Sender: TObject);
    procedure Panel1Paint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.Image1Paint(Sender: TObject);
begin
  with Image1.Canvas do
  begin
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    Pen.Style := psClear;
    Clear;
    Pen.Style := psSolid;
    Pen.Width := 2;
    Pen.Color := clHighlight;
    Line(-1, Image1.Height - 1, Image1.Width, Image1.Height - 1);
    Draw(Image1.Width div 2 - (Image1.Picture.Width div 2), Image1.Height div
      2 - (Image1.Picture.Height div 2), Image1.Picture.Graphic);
    Font.Color := clGray;
    Brush.Style := bsClear;
    TextOut(Image1.Width - TextWidth('Version: ' + Version) - 4,
      Image1.Height - TextHeight('Version: ' + Version) - 2, 'Version: ' + Version);
  end;
end;

procedure TAboutForm.HilfeClick(Sender: TObject);
begin
  OpenDocument(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'Doc'+PathDelim+'au3IDE.html');
end;

procedure TAboutForm.InfoMemoEnter(Sender: TObject);
begin
  Button2.SetFocus;
end;

procedure TAboutForm.Panel1Paint(Sender: TObject);
begin
  with Panel1 do
  begin
    Canvas.Pen.Color:=clHighlight;
    Canvas.Pen.Style:=psSolid;
    Canvas.Pen.Width:=2;
    Canvas.Line(-1, 1, Width, 1);
  end;
end;

end.
