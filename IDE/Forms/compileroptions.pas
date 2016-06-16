unit CompilerOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn;

type

  { TCompilerOptionsForm }

  TCompilerOptionsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    IDebugFileEdit: TFileNameEdit;
    ILogEdit: TEdit;
    COutputBox: TCheckBox;
    CAdvOutputBox: TCheckBox;
    CompilerConfigBox: TGroupBox;
    IOutputBox: TCheckBox;
    CLogEdit: TEdit;
    CDebugFileEdit: TFileNameEdit;
    CReleaseFileEdit: TFileNameEdit;
    IReleaseFileEdit: TFileNameEdit;
    InterpreterConfigBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure CDebugFileEditAcceptFileName(Sender: TObject; var Value: String);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  CompilerOptionsForm: TCompilerOptionsForm;

implementation

{$R *.lfm}

{ TCompilerOptionsForm }

procedure TCompilerOptionsForm.CDebugFileEditAcceptFileName(Sender: TObject;
  var Value: String);
begin
  CDebugFileEdit.InitialDir:=ExtractFilePath(Value);
  CReleaseFileEdit.InitialDir:=ExtractFilePath(Value);
  IDebugFileEdit.InitialDir:=ExtractFilePath(Value);
  IReleaseFileEdit.InitialDir:=ExtractFilePath(Value);
end;

end.

