unit OtherOptionsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Spin, Menus, LCLTranslator;

type

  { TOtherOptions }

  TOtherOptions = class(TForm)
    LangBox: TComboBox;
    Label6: TLabel;
    UpdateBox: TCheckBox;
    OKButton: TButton;
    IncVarBox: TCheckBox;
    CompOpenBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OKButton1: TButton;
    UndoBox: TSpinEdit;
    SortBox: TSpinEdit;
    WinWidthEdit: TSpinEdit;
    WinHeightEdit: TSpinEdit;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  OtherOptions: TOtherOptions;

implementation

{$R *.lfm}

end.

