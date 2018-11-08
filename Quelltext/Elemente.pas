unit Elemente;

interface

uses
  Windows, SysUtils, Forms, Controls, Classes, StdCtrls, ExtCtrls, DragDropFile,
  DragDrop, Menus, IniFiles, DCConst;

type
  TElementeForm = class(TForm)
    file_box: TListBox;
    bottom_panel: TPanel;
    btn_close: TButton;
    btn_remove: TButton;
    popup: TPopupMenu;
    add_file: TMenuItem;
    add_folder: TMenuItem;
    seperator: TMenuItem;
    remove: TMenuItem;
    tmr_refresh: TTimer;
    procedure form_show(Sender: TObject);
    procedure btn_remove_click(Sender: TObject);
    procedure btn_close_click(Sender: TObject);
    procedure form_create(Sender: TObject);
    procedure dropfile_drop(Sender: TObject; ShiftState: TShiftState;
  Point: TPoint; var Effect: Integer);
    procedure form_destroy(Sender: TObject);
    procedure add_folder_click(Sender: TObject);
    procedure add_file_click(Sender: TObject);
    procedure form_resize(Sender: TObject);
    procedure tmr_refreshTimer(Sender: TObject);
  private
    LangArray: array of TLanguageEntry;
    function GetLangEntry(name: string): string;
  public
    DropFile: TDropFileTarget;
  end;

var
  ElementeForm: TElementeForm;

implementation

uses Main;

{$R *.dfm}

function TElementeForm.GetLangEntry(name: string): string;
var
  i: integer;
begin
  for i := 0 to high(LangArray) do
  begin
    if LangArray[i].name = name then
    begin
      result := LangArray[i].text;
      break;
    end;
  end;
end;

procedure TElementeForm.tmr_refreshTimer(Sender: TObject);
begin
  tmr_refresh.Enabled := false;

  file_box.Repaint;
end;

procedure TElementeForm.btn_close_click(Sender: TObject);
begin
  close;
end;

procedure TElementeForm.btn_remove_click(Sender: TObject);
var
  i: integer;
begin
  try
    mainform.KaZip.close;
  except

  end;

  mainform.KaZip.Open(mainform.fileopen);
  try
    for i := 0 to file_box.Items.Count - 1 do
    begin
      if file_box.Selected[i] then
        mainform.KaZip.Remove(file_box.Items.Strings[i]);
    end;

    // form_show(self);
    file_box.Clear;
    for i := 0 to mainform.KaZip.FileNames.Count - 1 do
      file_box.Items.Add(mainform.KaZip.FileNames.Strings[i]);

    mainform.edt_vle4.Text := mainform.BestimmeDateiGroesse(mainform.fileopen);
  finally
    mainform.KaZip.close;
  end;
end;

procedure TElementeForm.add_file_click(Sender: TObject);
begin
  mainform.m_dateiensammlung_add_file.Click;
  form_show(self);
end;

procedure TElementeForm.form_create(Sender: TObject);
var
  ini: TIniFile;
  str: TStringList;
  i: integer;
begin
  // Sprachdatei auslesen

  ini := TIniFile.Create(ExtractFilePath(Application.ExeName)+'Language.ini');
  str := TStringList.Create();
  try
    ini.ReadSection(Name, str);
    for i := 0 to str.count-1 do
    begin
      setlength(LangArray, length(LangArray)+1);
      LangArray[length(LangArray)-1].name := str.strings[i];
      LangArray[length(LangArray)-1].text := ini.ReadString(name, str.strings[i], '?');
      LangArray[length(LangArray)-1].text := StringReplace(LangArray[length(LangArray)-1].text, '###', #13#10, [rfReplaceAll]);
    end;
  finally
    ini.free;
    str.Free;
  end;

  // Form vorbereiten

  DropFile := TDropFileTarget.Create(self);
  DropFile.Dragtypes := [dtCopy];
  DropFile.OnDrop := dropfile_drop;

  DropFile.Register(elementeform);

  popup.Images := mainform.imagelist;

  btn_close.caption := GetLangEntry('close');
  btn_remove.caption := GetLangEntry('remove');
  caption := GetLangEntry('caption');

  add_file.Caption := GetLangEntry('add_file');
  add_folder.Caption := GetLangEntry('add_folder');
  remove.Caption := GetLangEntry('remove');
end;

procedure TElementeForm.dropfile_drop(Sender: TObject; ShiftState: TShiftState;
  Point: TPoint; var Effect: Integer);
var
  i: integer;
begin
  effect := DROPEFFECT_NONE; // damit stürzt Windows 95 nicht ab

  mainform.steuerelementesperren(true);
  mainform.zeige_wartenform(true);
  try

  for i := 0 to DropFile.Files.Count - 1 do
  begin
    mainform.addtozip(DropFile.Files.Strings[i], mainform.fileopen);
  end;

  mainform.edt_vle4.Text := mainform.BestimmeDateiGroesse(mainform.fileopen);
  form_show(self);

  finally
  mainform.steuerelementesperren(false);
  mainform.zeige_wartenform(false);
  tmr_refresh.Enabled := true;
  end;
end;

procedure TElementeForm.form_resize(Sender: TObject);
begin
  btn_remove.Left := bottom_panel.width - btn_remove.Width - 8;
end;

procedure TElementeForm.form_destroy(Sender: TObject);
begin
  try
    dropfile.free;
  except
  end;
end;

procedure TElementeForm.form_show(Sender: TObject);
var
  i: integer;
begin
  try
    mainform.KaZip.close;
  except

  end;

  mainform.KaZip.Open(mainform.fileopen);
  try
    file_box.Clear;
    for i := 0 to mainform.KaZip.FileNames.Count - 1 do
      file_box.Items.Add(mainform.KaZip.FileNames.Strings[i]);
  finally
    mainform.KaZip.close;
  end;
end;

procedure TElementeForm.add_folder_click(Sender: TObject);
begin
  mainform.m_dateiensammlung_add_folder.Click;
  form_show(self);
end;

end.
