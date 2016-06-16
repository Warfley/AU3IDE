program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, Unit1, Editor, UnitParser, AU3Types, ListRecords, 
CodeFormatter, ToolTip, FormEditor
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='AU3 Editor';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

