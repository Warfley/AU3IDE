unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs, CheckLst,
  EditBtn, ExtCtrls, StdCtrls, sha1;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckListBox1: TCheckListBox;
    DirectoryEdit1: TDirectoryEdit;
    DirectoryEdit2: TDirectoryEdit;
    Edit1: TEdit;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure CheckListBox1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function GetHash(FileName: string): string; inline;
begin
  Result := SHA1Print(SHA1File(FileName));
end;

procedure TForm1.DirectoryEdit1Change(Sender: TObject);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    FindAllFiles(sl, IncludeTrailingPathDelimiter(DirectoryEdit1.Directory), '*.*');
    CheckListBox1.Clear;
    for i := 0 to sl.Count - 1 do
      CheckListBox1.Items.Add(CreateRelativePath(sl[i], DirectoryEdit1.Directory));
  finally
    sl.Free;
  end;
  CheckListBox1.CheckAll(cbChecked);
end;

procedure TForm1.Edit1Enter(Sender: TObject);
begin
  Edit1.SelectAll;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: integer;
  sl: TStringList;
begin
  if DirectoryEdit2.Directory = '' then
    exit;
  sl := TStringList.Create;
  try
    sl.Add(Edit1.Text);
    for i := 0 to CheckListBox1.Items.Count - 1 do
      if CheckListBox1.Checked[i] then
      begin
        ForceDirectories(ExtractFilePath(CreateAbsolutePath(CheckListBox1.Items[i],
          DirectoryEdit2.Directory)));
        CopyFile(CreateAbsolutePath(CheckListBox1.Items[i], DirectoryEdit1.Directory),
          CreateAbsolutePath(CheckListBox1.Items[i], DirectoryEdit2.Directory));
        sl.Values[CheckListBox1.Items[i]] :=
          GetHash(CreateAbsolutePath(CheckListBox1.Items[i], DirectoryEdit1.Directory));
      end;
    sl.Insert(1, IntToStr(sl.Count));
    sl.SaveToFile(CreateAbsolutePath('Update.txt', DirectoryEdit2.Directory));
    CheckListBox1.Items.SaveToFile(CreateAbsolutePath('Install.txt', DirectoryEdit2.Directory));
  finally
    sl.Free;
  end;
end;

procedure TForm1.CheckListBox1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = ord('A') then
    CheckListBox1.CheckAll(cbChecked);
end;

end.
