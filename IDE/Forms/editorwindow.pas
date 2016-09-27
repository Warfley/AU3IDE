unit EditorWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, Menus, LCLTranslator;

type

  { TEditorViewForm }

  TEditorViewForm = class(TForm)
    EditorControl: TPageControl;
    OpenEditorButton2: TSpeedButton;
    ToolBar2: TToolBar;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  EditorViewForm: TEditorViewForm;

implementation

{$R *.lfm}

end.

