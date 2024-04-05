{******************************************************************************}
{                                                                              }
{  Main form used by two examples:                                             }
{  StyledTaskDialogDemo and AnimatedTaskDialogDemo                             }
{                                                                              }
{  Copyright (c) 2022-2024 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{  Contributors:                                                               }
{                                                                              }
{  https://github.com/EtheaDev/StyledComponents                                }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit MainFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids, DBGrids, DB, DBClient, StdCtrls, DBCtrls,
  Vcl.CheckLst, Vcl.StyledTaskDialog, UITypes, Vcl.ButtonStylesAttributes;

{$WARN SYMBOL_PLATFORM OFF}
type
  TMainForm = class(TForm)
    TaskDialog: TTaskDialog;
    StyledTaskDialog: TStyledTaskDialog;
    ExtraGroupBox: TGroupBox;
    ExpandedTextLabel: TLabel;
    ExpandedTextMemo: TMemo;
    btUseStyledDialogComp: TButton;
    btUseNativeDialogComp: TButton;
    FooterTextLabel: TLabel;
    FooterTextMemo: TMemo;
    Label1: TLabel;
    VerificationTextMemo: TMemo;
    rgMainIcon: TRadioGroup;
    CaptionLabel: TLabel;
    CaptionEdit: TEdit;
    ClientPanel: TPanel;
    StyleLabel: TLabel;
    cbChangeStyle: TComboBox;
    LeftPanelClient: TPanel;
    TitleLabel: TLabel;
    edTitle: TEdit;
    TextMessageLabel: TLabel;
    edMessage: TMemo;
    FontLabel: TLabel;
    FontComboBox: TComboBox;
    btCustomTaskDialog: TButton;
    btNativeTaskDialog: TButton;
    btRaiseErrorTaskDialog: TButton;
    btCustomMsgDialog: TButton;
    btNativeMsgDialog: TButton;
    btRaiseErrorMsgDialog: TButton;
    RightPanel: TPanel;
    DefaultButtonLabel: TLabel;
    rgDlgType: TRadioGroup;
    clbButtons: TCheckListBox;
    DefaultButtonComboBox: TComboBox;
    FamilyComboBox: TComboBox;
    Label2: TLabel;
    gbResult: TGroupBox;
    MRLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ShowDlg(Sender: TObject);
    procedure RaiseError(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TaskDialogTimer(Sender: TObject; TickCount: Cardinal;
      var Reset: Boolean);
    procedure UseStyleDialogCompClick(Sender: TObject);
    procedure cbUseStyledDialogClick(Sender: TObject);
    procedure FontComboBoxSelect(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
    procedure FamilyComboBoxSelect(Sender: TObject);
    procedure RaiseDatabaseError(Sender: TObject);
  private
    procedure ShowSelection(const AModalResult: TModalResult);
    procedure BuildStyleList;
  protected
    procedure Loaded; override;
  public
    procedure ShowError(Sender: TObject; E: Exception);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  System.TypInfo
  , Vcl.Themes
  , Vcl.StyledCmpMessages
  , Vcl.StyledCmpStrUtils
  , Vcl.ButtonStylesAttributes
  , Vcl.StyledTaskDialogFormUnit;

procedure TMainForm.BuildStyleList;
var
  i, SelectedIndex: integer;
  LStyleName, LActiveStyleName: string;
begin
  SelectedIndex := -1;
  cbChangeStyle.Items.Clear;
  LActiveStyleName := TStyleManager.ActiveStyle.Name;
  for i := 0 to High(TStyleManager.StyleNames) do
  begin
    LStyleName := TStyleManager.StyleNames[i];
    cbChangeStyle.Items.Add(LStyleName);
    if SameText(LStyleName, LActiveStyleName)  then
      SelectedIndex := i;
  end;
  cbChangeStyle.ItemIndex := SelectedIndex;
end;

procedure TMainForm.cbChangeStyleSelect(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    TStyleManager.TrySetStyle(cbChangeStyle.Text);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.cbUseStyledDialogClick(Sender: TObject);
begin
//  UseStyledDialogForm(cbUseStyledDialog.Checked)
end;

procedure TMainForm.FamilyComboBoxSelect(Sender: TObject);
begin
  InitializeStyledTaskDialogs(True, Screen.MessageFont, FamilyComboBox.Text);
end;

procedure TMainForm.FontComboBoxSelect(Sender: TObject);
begin
  Screen.MessageFont.Name := FontComboBox.Text;
  InitializeStyledTaskDialogs(True, Screen.MessageFont, FamilyComboBox.Text);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  dt : TMsgDlgType;
  db : TMsgDlgBtn;
  Msg: string;
  LButtonName: string;
begin
  //Example to resize message font
  //Screen.MessageFont.Size := Round(Screen.MessageFont.Size * 1.2);

  Caption := Application.Title;
  MRLabel.Font.Style := [TFontStyle.fsBold];
  FontComboBox.Items.Assign(Screen.Fonts);
  FontComboBox.Text := Screen.IconFont.Name;
  BuildStyleList;
  SetUseAlwaysTaskDialog(True);

  for dt := Low(TMsgDlgType) to High(TMsgDlgType)  do
    rgDlgType.Items.Add(GetEnumName(TypeInfo(TMsgDlgType), Ord(dt)));
  for db := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  begin
    LButtonName := GetEnumName(TypeInfo(TMsgDlgBtn), Ord(db));
    clbButtons.Items.Add(LButtonName);
    if db = mbOK then
      clbButtons.Checked[Ord(db)] := True;
    DefaultButtonComboBox.Items.Add(LButtonName);
  end;

  rgDlgType.ItemIndex := 0;

  Msg :='The file was created: '+StringToHRef('C:\Windows\System32\license.rtf','license.rtf')+sLineBreak+
        'You can run: '+StringToHRef('C:\Windows\System32\Notepad.exe','Notepad Editor')+sLineBreak+
        'You can open folder: '+StringToHRef('C:\Windows\System32\')+sLineBreak+
        'You can visit site: '+StringToHRef('http://www.ethea.it','www.Ethea.it');

  edMessage.Text := Msg;
  //UseStyledDialogForm(cbUseStyledDialog.Checked);
end;

procedure TMainForm.ShowSelection(const AModalResult: TModalResult);
var
  LResultMsg: string;
begin
  case AModalResult of
    mrYes: LResultMsg := 'Yes';
    mrNo: LResultMsg := 'No';
    mrOk: LResultMsg := 'OK';
    mrCancel: LResultMsg := 'Cancel';
    mrAbort: LResultMsg := 'Abort';
    mrRetry: LResultMsg := 'Retry';
    mrIgnore: LResultMsg := 'Ignore';
    mrAll: LResultMsg := 'All';
    mrNoToAll: LResultMsg := 'NoToAll';
    mrYesToAll: LResultMsg := 'YesToAll';
    mrClose: LResultMsg := 'Close';
  else
    LResultMsg := '';
  end;
  if LResultMsg <> '' then
    MRLabel.Caption :=  Format('"%s"', [LResultMsg]);
end;

procedure TMainForm.ShowDlg(Sender: TObject);
var
  Buttons: TMsgDlgButtons;
  db : TMsgDlgBtn;
  LDefaultButton: TMsgDlgBtn;
  LUseDefault: boolean;
  LResult: TModalResult;
begin
  Buttons := [];
  LUseDefault := False;
  LDefaultButton := mbOK;
  HelpContext := 0;
  for db := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if clbButtons.Checked[Ord(db)] then
    begin
      Buttons := Buttons + [db];
      if db = TMsgDlgBtn.mbHelp then
        HelpContext := 200;
      if SameText(DefaultButtonComboBox.Text, clbButtons.Items[Ord(db)]) then
      begin
        LDefaultButton := db;
        LUseDefault := True;
      end;
    end;

  if LUseDefault then
  begin
    if Sender = btCustomTaskDialog then
      LResult := StyledTaskDlgPos(edTitle.Text, edMessage.Text, TMsgDlgType(rgDlgType.ItemIndex), Buttons, LDefaultButton, HelpContext)
    else if Sender = btNativeTaskDialog then
      LResult := TaskMessageDlg(edTitle.Text, edMessage.Text, TMsgDlgType(rgDlgType.ItemIndex), Buttons, HelpContext)
    else if Sender = btNativeMsgDialog then
      LResult := MessageDlg(edMessage.Text, TMsgDlgType(rgDlgType.ItemIndex), Buttons, 0)
    else if Sender = btCustomMsgDialog then
      LResult := StyledMessageDlgPos(edMessage.Text, TMsgDlgType(rgDlgType.ItemIndex), Buttons, LDefaultButton, HelpContext)
    else
      LResult := mrNone;
  end
  else
  begin
    if Sender = btCustomTaskDialog then
      LResult := StyledTaskDlgPos(edTitle.Text, edMessage.Text, TMsgDlgType(rgDlgType.ItemIndex), Buttons, HelpContext)
    else if Sender = btNativeTaskDialog then
      LResult := TaskMessageDlg(edTitle.Text, edMessage.Text, TMsgDlgType(rgDlgType.ItemIndex), Buttons, HelpContext)
    else if Sender = btNativeMsgDialog then
      LResult := MessageDlg(edMessage.Text, TMsgDlgType(rgDlgType.ItemIndex), Buttons, 0)
    else if Sender = btCustomMsgDialog then
      LResult := StyledMessageDlgPos(edMessage.Text, TMsgDlgType(rgDlgType.ItemIndex), Buttons, HelpContext)
    else
      LResult := mrNone;
  end;
  ShowSelection(LResult);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ;
end;

procedure TMainForm.Loaded;
begin
  //Font.Assign(Screen.IconFont);
  //Screen.MessageFont.Assign(Font);
  inherited;
end;



procedure TMainForm.RaiseDatabaseError(Sender: TObject);
begin
  DatabaseError('Unexpected Error!'+sLineBreak+
    'Please read the error file: '+
    StringToHRef('C:\Windows\System32\license.rtf','license.rtf'));
end;

procedure TMainForm.RaiseError(Sender: TObject);
begin
  raise Exception.CreateHelp('Unexpected Error!'+sLineBreak+
    'Please read the error file: '+
    StringToHRef('C:\Windows\System32\license.rtf','license.rtf'),
      200);
end;

procedure TMainForm.ShowError(Sender: TObject; E: Exception);
var
  Buttons: TMsgDlgButtons;
  Selection: Integer;
  LTitle: string;
  LMessage: string;
  LHelpContext: Integer;
  LUseTaskDialog: Boolean;
begin
  LTitle := GetErrorClassNameDesc(E.ClassName,
    E is EAccessViolation);
  LMessage := E.Message;
  LHelpContext := 0;

  LUseTaskDialog := (E.HelpContext = -100);

  if E.HelpContext <> 0 then
    LHelpContext := Abs(E.HelpContext);

  Buttons := [mbOK];
  if LHelpContext <> 0 then
    Buttons := Buttons + [mbHelp];
  if LUseTaskDialog and (Win32MajorVersion >= 6) then
  begin
    if E.InheritsFrom(EAccessViolation) then
      Buttons := Buttons + [mbAbort];

    Selection := StyledTaskDlgPos(LTitle, LMessage, mtError, Buttons, mbOK, LHelpContext);
    if Selection = mrAbort then
      Application.Terminate
    else if Selection = -1 then
      Application.HelpContext(LHelpContext);
  end
  else
  begin
    Selection := StyledMessageDlgPos(LMessage, mtError, Buttons, mbOK, LHelpContext);
    if Selection = mrAbort then
      Application.Terminate
    else if Selection = -1 then
      Application.HelpContext(LHelpContext);
  end;
end;

procedure TMainForm.UseStyleDialogCompClick(Sender: TObject);
const
  LModalResults: array[TMsgDlgBtn] of Integer = (mrYes, mrNo, mrOk, mrCancel,
    mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll, mrYesToAll, -1, mrClose);

var
  LTaskDialog: TTaskDialog;
  LDefaultButton: Boolean;
  db : TMsgDlgBtn;
  LTaskDialogBaseButtonItem: TTaskDialogBaseButtonItem;

  procedure AddCommonButton(const AButton: TTaskDialogCommonButton);
  begin
    LTaskDialog.CommonButtons := LTaskDialog.CommonButtons + [AButton];
    if LDefaultButton then
      LTaskDialog.DefaultButton := AButton;
  end;

begin
  if Sender = btUseStyledDialogComp then
    LTaskDialog := StyledTaskDialog
  else
    LTaskDialog := TaskDialog;

  LTaskDialog.Buttons.Clear;
  LTaskDialog.CommonButtons := [];
  for db := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  begin
    if clbButtons.Checked[Ord(db)] then
    begin
      LDefaultButton := SameText(DefaultButtonComboBox.Text, clbButtons.Items[Ord(db)]);
      //tcbOk, tcbYes, tcbNo, tcbCancel, tcbRetry, tcbClose
      case db of
        TMsgDlgBtn.mbYes: AddCommonButton(tcbYes);
        TMsgDlgBtn.mbNo: AddCommonButton(tcbNo);
        TMsgDlgBtn.mbOK: AddCommonButton(tcbOK);
        TMsgDlgBtn.mbCancel: AddCommonButton(tcbCancel);
        TMsgDlgBtn.mbRetry: AddCommonButton(tcbRetry);
        TMsgDlgBtn.mbClose: AddCommonButton(tcbClose);
      else
        LTaskDialogBaseButtonItem := LTaskDialog.Buttons.add;
        LTaskDialogBaseButtonItem.Caption := GetEnumName(TypeInfo(TMsgDlgBtn), Ord(db));
        LTaskDialogBaseButtonItem.ModalResult := LModalResults[db];
        if LDefaultButton then
          LTaskDialogBaseButtonItem.Default := True;
      end;
    end;
  end;

  LTaskDialog.Caption := CaptionEdit.Text;
  LTaskDialog.Title := edTitle.Text;
  LTaskDialog.Text := edMessage.Text;
  LTaskDialog.FooterText := FooterTextMemo.Text;
  LTaskDialog.ExpandedText := ExpandedTextMemo.Text;
  LTaskDialog.VerificationText := VerificationTextMemo.Text;
  LTaskDialog.MainIcon := rgMainIcon.ItemIndex;
  LTaskDialog.Execute(Self.Handle);
  ShowSelection(LTaskDialog.ModalResult);
end;

procedure TMainForm.TaskDialogTimer(Sender: TObject; TickCount: Cardinal; var Reset: Boolean);
begin
   // TaskDialog1.ProgressBar.Position := MyThread.CurrentProgressPercent;
   // Demo
   //TaskDialog.ProgressBar.Position :=  TaskDialog.ProgressBar.Position + 1;
   TaskDialog.Execute(Self.Handle);
end;

initialization
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

end.

