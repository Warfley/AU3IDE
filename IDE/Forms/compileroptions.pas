unit CompilerOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, LCLTranslator;

type

  { TCompilerOptionsForm }

  TCompilerOptionsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    SaveIntBox: TCheckBox;
    PathEdit: TDirectoryEdit;
    Label1: TLabel;
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

end.

