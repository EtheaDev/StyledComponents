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
  Vcl.CheckLst, Vcl.StyledTaskDialog, UITypes, Vcl.ButtonStylesAttributes,
  Vcl.Samples.Spin;

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
    btStyledTaskDialog: TButton;
    btNativeTaskDialog: TButton;
    btRaiseDatabaseError: TButton;
    btStyledMsgDialog: TButton;
    btNativeMsgDialog: TButton;
    btRaiseGenericError: TButton;
    RightPanel: TPanel;
    DefaultButtonLabel: TLabel;
    rgDlgType: TRadioGroup;
    clbButtons: TCheckListBox;
    DefaultButtonComboBox: TComboBox;
    FamilyComboBox: TComboBox;
    Label2: TLabel;
    gbResult: TGroupBox;
    MRLabel: TLabel;
    AlphaBlendSpinEdit: TSpinEdit;
    AlphaLabel: TLabel;
    cbUseCommandLinks: TCheckBox;
    cbUseTitleInMessageDlg: TCheckBox;
    btNativeShowMessage: TButton;
    btStyledShowMessage: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ShowDlg(Sender: TObject);
    procedure RaiseError(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TaskDialogTimer(Sender: TObject; TickCount: Cardinal;
      var Reset: Boolean);
    procedure UseStyleDialogCompClick(Sender: TObject);
    procedure FontComboBoxSelect(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
    procedure FamilyComboBoxSelect(Sender: TObject);
    procedure RaiseDatabaseError(Sender: TObject);
    procedure AlphaBlendSpinEditChange(Sender: TObject);
    procedure TaskDialogButtonClicked(Sender: TObject;
      ModalResult: TModalResult; var CanClose: Boolean);
    procedure TaskDialogDialogConstructed(Sender: TObject);
    procedure TaskDialogDialogCreated(Sender: TObject);
    procedure TaskDialogDialogDestroyed(Sender: TObject);
    procedure TaskDialogExpanded(Sender: TObject);
    procedure TaskDialogHyperlinkClicked(Sender: TObject);
    procedure TaskDialogNavigated(Sender: TObject);
    procedure TaskDialogRadioButtonClicked(Sender: TObject);
    procedure TaskDialogVerificationClicked(Sender: TObject);
    procedure cbUseClick(Sender: TObject);
    procedure DoShowMessage(Sender: TObject);
  private
    FRadioButtonSelected: string;
    procedure ShowSelection(const AModalResult: TModalResult);
    procedure BuildStyleList;
    procedure InitializeDialogs;
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
  , Winapi.ShellAPI
  , Vcl.Themes
  , Vcl.StyledCmpMessages
  , Vcl.StyledCmpStrUtils
  , Vcl.StyledTaskDialogFormUnit;

procedure TMainForm.AlphaBlendSpinEditChange(Sender: TObject);
begin
  InitializeDialogs;
end;

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

procedure TMainForm.cbUseClick(Sender: TObject);
begin
  InitializeDialogs;
end;

procedure TMainForm.DoShowMessage(Sender: TObject);
begin
  if Sender = btNativeShowMessage then
    ShowMessage(edMessage.Text)
  else
    StyledShowMessage(edMessage.Text);
end;

procedure TMainForm.InitializeDialogs;
begin
  InitializeStyledTaskDialogs(cbUseTitleInMessageDlg.Checked,
    Screen.MessageFont,
    cbUseCommandLinks.Checked,
    FamilyComboBox.Text, AlphaBlendSpinEdit.Value);
end;

procedure TMainForm.FamilyComboBoxSelect(Sender: TObject);
begin
  InitializeDialogs;
end;

procedure TMainForm.FontComboBoxSelect(Sender: TObject);
begin
  Screen.MessageFont.Name := FontComboBox.Text;
  InitializeDialogs;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  dt : TMsgDlgType;
  db : TMsgDlgBtn;
  Msg: string;
  LButtonName: string;
begin
  //Example to resize and change message font
  //Screen.MessageFont.Size := Round(Screen.MessageFont.Size * 1.2);
  //Screen.MessageFont.Name := 'Century Gothic';
  InitializeDialogs;

  Caption := Application.Title;
  MRLabel.Font.Style := [TFontStyle.fsBold];
  FontComboBox.Items.Assign(Screen.Fonts);
  FontComboBox.Text := Screen.IconFont.Name;
  BuildStyleList;

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
  LButtons: TMsgDlgButtons;
  db : TMsgDlgBtn;
  LDefaultButton: TMsgDlgBtn;
  LUseDefaultbutton: boolean;
  LResult: TModalResult;
  LDlgType: TMsgDlgType;
begin
  LButtons := [];
  LUseDefaultbutton := False;
  LDefaultButton := mbOK;
  LDlgType := TMsgDlgType(rgDlgType.ItemIndex);
  //HelpContext := 0;
  for db := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if clbButtons.Checked[Ord(db)] then
    begin
      LButtons := LButtons + [db];
      if db = TMsgDlgBtn.mbHelp then
        HelpContext := 200;
      if SameText(DefaultButtonComboBox.Text, clbButtons.Items[Ord(db)]) then
      begin
        //The Default Button correponds to a selected Button
        LDefaultButton := db;
        LUseDefaultbutton := True;
      end;
    end;

  if LUseDefaultbutton then
  begin
    if Sender = btNativeTaskDialog then
      //Call Delphi TaskMessageDlg passing Default Button
      LResult := TaskMessageDlg(edTitle.Text, edMessage.Text, LDlgType, LButtons, HelpContext, LDefaultButton)
    else if Sender = btNativeMsgDialog then
      //Call Delphi MessageDlg (ignore Title!) passing Default Button
      LResult := MessageDlg(edMessage.Text, LDlgType, LButtons, 0, LDefaultButton)
    else if Sender = btStyledTaskDialog then
      //Call StyledTaskMessageDlg passing Default Button
      LResult := StyledTaskMessageDlg(edTitle.Text, edMessage.Text, LDlgType, LButtons, HelpContext, LDefaultButton)
    else if Sender = btStyledMsgDialog then
      //Call StyledMessageDlg (ignore Title!) passing Default Button
      LResult := StyledMessageDlg(edMessage.Text, LDlgType, LButtons, HelpContext, LDefaultButton)
    else
      LResult := mrNone;
  end
  else
  begin
    if Sender = btNativeTaskDialog then
      //Call Delphi TaskMessageDlg without Default Button
      LResult := TaskMessageDlg(edTitle.Text, edMessage.Text, LDlgType, LButtons, HelpContext)
    else if Sender = btNativeMsgDialog then
      //Call Delphi MessageDlg (ignore Title!) without Default Button
      LResult := MessageDlg(edMessage.Text, LDlgType, LButtons, 0)
    else if Sender = btStyledTaskDialog then
      //Call StyledTaskMessageDlg without Default Button
      LResult := StyledTaskMessageDlg(edTitle.Text, edMessage.Text, LDlgType, LButtons, HelpContext)
    else if Sender = btStyledMsgDialog then
      //Call StyledMessageDlgPos (ignore Title!) without Default Button
      LResult := StyledMessageDlg(edMessage.Text, LDlgType, LButtons, HelpContext)
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

    Selection := StyledTaskMessageDlg(LTitle, LMessage, mtError, Buttons, LHelpContext, mbOK);
    if Selection = mrAbort then
      Application.Terminate
    else if Selection = -1 then
      Application.HelpContext(LHelpContext);
  end
  else
  begin
    Selection := StyledMessageDlg(LMessage, mtError, Buttons, LHelpContext, mbOK);
    if Selection = mrAbort then
      Application.Terminate
    else if Selection = -1 then
      Application.HelpContext(LHelpContext);
  end;
end;

procedure TMainForm.UseStyleDialogCompClick(Sender: TObject);
const
  LModalResults: array[TMsgDlgBtn] of Integer = (mrYes, mrNo, mrOk, mrCancel,
    mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll, mrYesToAll, mrHelp, mrClose);

var
  LTaskDialog: TTaskDialog;
  LDefaultButton: Boolean;

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

  (* remove comment if you want to use buttons defines in the form
     or leave the comment to use the custom buttons defines at design-time
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
        LTaskDialogBaseButtonItem.Caption := Copy(GetEnumName(TypeInfo(TMsgDlgBtn), Ord(db)), 3, MaxInt);
        LTaskDialogBaseButtonItem.ModalResult := LModalResults[db];
        if LDefaultButton then
          LTaskDialogBaseButtonItem.Default := True;
      end;
    end;
  end;
  *)
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

procedure TMainForm.TaskDialogButtonClicked(Sender: TObject;
  ModalResult: TModalResult; var CanClose: Boolean);
begin
  ;
end;

procedure TMainForm.TaskDialogDialogConstructed(Sender: TObject);
begin
  ;
end;

procedure TMainForm.TaskDialogDialogCreated(Sender: TObject);
begin
  ;
end;

procedure TMainForm.TaskDialogDialogDestroyed(Sender: TObject);
begin
  ;
end;

procedure TMainForm.TaskDialogExpanded(Sender: TObject);
begin
  ;
end;

procedure TMainForm.TaskDialogHyperlinkClicked(Sender: TObject);
var
  LURL: String;
begin
  LURL := (Sender as TTaskDialog).URL;
  ShellExecute(Self.Handle, 'open' , PChar(LURL), nil, nil, SW_SHOW );
end;

procedure TMainForm.TaskDialogNavigated(Sender: TObject);
begin
  ;
end;

procedure TMainForm.TaskDialogRadioButtonClicked(Sender: TObject);
var
  LTaskDialog: TCustomTaskDialog;
begin
  if Sender is TCustomTaskDialog then
    LTaskDialog := TCustomTaskDialog(Sender)
  else
    LTaskDialog := nil;
  if Assigned(LTaskDialog) then
    FRadioButtonSelected := LTaskDialog.RadioButton.Caption;
end;

procedure TMainForm.TaskDialogTimer(Sender: TObject; TickCount: Cardinal; var Reset: Boolean);
begin
   // TaskDialog1.ProgressBar.Position := MyThread.CurrentProgressPercent;
   // Demo
   //TaskDialog.ProgressBar.Position :=  TaskDialog.ProgressBar.Position + 1;
   TaskDialog.Execute(Self.Handle);
end;

procedure TMainForm.TaskDialogVerificationClicked(Sender: TObject);
begin
  if tfVerificationFlagChecked in (Sender as TTaskDialog).Flags then
    ShowMessage('Verification Click TRUE')
  else
    ShowMessage('Verification Click FALSE');
end;

initialization
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

end.

