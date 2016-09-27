unit aboutautoit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, windows, Graphics, Dialogs, ExtCtrls,LCLTranslator;

type

  { TAboutAutoitForm }

  TAboutAutoitForm = class(TForm)
    Timer1: TTimer;
    procedure FormHide(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    h: integer;
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutAutoitForm: TAboutAutoitForm;

implementation

{$R *.lfm}

{ TAboutAutoitForm }
function HSVtoRGB(H: integer; S, V: byte): TColor;
var
  ht, d, t1, t2, t3: integer;
  R, G, B: word;
begin
  if S = 0 then
  begin
    R := V;
    G := V;
    B := V;
  end
  else
  begin
    ht := H * 6;
    d := ht mod 360;

    t1 := round(V * (255 - S) / 255);
    t2 := round(V * (255 - S * d / 360) / 255);
    t3 := round(V * (255 - S * (360 - d) / 360) / 255);

    case ht div 360 of
      0:
      begin
        R := V;
        G := t3;
        B := t1;
      end;
      1:
      begin
        R := t2;
        G := V;
        B := t1;
      end;
      2:
      begin
        R := t1;
        G := V;
        B := t3;
      end;
      3:
      begin
        R := t1;
        G := t2;
        B := V;
      end;
      4:
      begin
        R := t3;
        G := t1;
        B := V;
      end;
      else
      begin
        R := V;
        G := t1;
        B := t2;
      end;
    end;
  end;
  Result := RGB(R, G, B);
end;

procedure TAboutAutoitForm.FormHide(Sender: TObject);
begin
  Timer1.Enabled:=False;
end;

procedure TAboutAutoitForm.FormPaint(Sender: TObject);
var r: Integer;
begin
     if GetAsyncKeyState(VK_ESCAPE) and 1 <> 0 then
      Close;
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := HSVtoRGB(h, 255, 255);
    pen.Style := psClear;
    r:=(Self.Height-40) div 4;
    Ellipse(Self.Width div 2 - r, Self.Height -r-40, Self.Width div 2, Self.Height-40);
    Ellipse(Self.Width div 2, Self.Height-r-40, Self.Width div 2 +r, Self.Height-40);
    Rectangle(Self.Width div 2-r div 2, r div 2, Self.Width div 2+r div 2, Self.Height-(r div 2)-40);
    Ellipse(Self.Width div 2-r div 2, 0, Self.Width div 2+r div 2, r);
    Brush.Style:=bsClear;
    Font.Color:=not Brush.Color shl 4 shr 4;
    Font.Height:=40;
    TextOut(Self.Width div 2- TextWidth('GAAAAAAAYYYYYYY') div 2, Self.Height-40,'GAAAAAAAYYYYYYY');
  end;
end;

procedure TAboutAutoitForm.FormResize(Sender: TObject);
var r: Integer;
  b: TBitmap;
begin
  b:=TBitmap.Create;
  try
  b.Monochrome:=True;
  b.Width:=Self.Width;
  b.Height:=Self.Height;
  with b.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    pen.Style := psClear;
    r:=(Self.Height-40) div 4;
    Ellipse(Self.Width div 2 - r, Self.Height -r-40, Self.Width div 2, Self.Height-40);
    Ellipse(Self.Width div 2, Self.Height-r-40, Self.Width div 2 +r, Self.Height-40);
    Rectangle(Self.Width div 2-r div 2, r div 2, Self.Width div 2+r div 2, Self.Height-(r div 2)-40);
    Ellipse(Self.Width div 2-r div 2, 0, Self.Width div 2+r div 2, r);
    Brush.Style:=bsClear;
    Font.Color:=clWhite;
    Font.Height:=40;
    TextOut(Self.Width div 2- TextWidth('GAAAAAAAYYYYYYY') div 2, Self.Height-40,'GAAAAAAAYYYYYYY');
  end;
  SetShape(b);
  finally
    b.Free;
  end;
end;

procedure TAboutAutoitForm.FormShow(Sender: TObject);
begin
  WindowState:=wsMaximized;
  Timer1.Enabled:=True;
end;

procedure TAboutAutoitForm.Timer1Timer(Sender: TObject);
begin
  h:=(h+1) mod 360;
  Invalidate;
end;

end.
