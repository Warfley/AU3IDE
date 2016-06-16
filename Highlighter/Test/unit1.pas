unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterJScript, Forms,
  Controls, Graphics, Dialogs, StdCtrls, AU3Highlighter;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    SynEdit1: TSynEdit;
    a: TAU3SynHighlight;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    HL: array[0..255] of TList;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  a := TAU3SynHighlight.Create(self);
  a.LoadConfig(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'HL');
  SynEdit1.Highlighter := a;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(a) then
    a.Free;
end;

end.

