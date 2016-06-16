unit SampeProjectView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ListFilterEdit;

type

  { TSampleForm }

  TSampleForm = class(TForm)
    ListBox1: TListBox;
    ListFilterEdit1: TListFilterEdit;
    procedure FormShow(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    FSelected: string;
    { private declarations }
  public
    property Selected: string read FSelected write FSelected;
    { public declarations }
  end;

var
  SampleForm: TSampleForm;

implementation

{$R *.lfm}

{ TSampleForm }

procedure TSampleForm.FormShow(Sender: TObject);
var
  sl: TStringList;
  i: integer;
begin
  FSelected := '';
  sl := TStringList.Create;
  try
    FindAllFiles(sl, IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'Examples', '*.au3proj');
    for i := 0 to sl.Count - 1 do
      ListBox1.Items.Add(CreateRelativePath(sl[i], ExtractFilePath(ParamStr(0))));
  finally
    sl.Free;
  end;
end;

procedure TSampleForm.ListBox1DblClick(Sender: TObject);
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    FSelected := CreateAbsolutePath(ListBox1.Items[ListBox1.ItemIndex],
      ExtractFilePath(ParamStr(0)));
    Close;
  end;
end;

end.
