/// <summary>
///  StyledTaskDialog and the message hooks. No modal dialog is ever shown here.
/// </summary>
unit StyledDialogTests;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Dialogs,
  DUnitX.TestFramework,
  Vcl.StyledTaskDialog,
  StyledTestUtils;

type
  [TestFixture]
  TStyledTaskDialogSmokeTests = class
  public
    /// <summary>Sanity: a fresh dialog has no custom buttons.</summary>
    [Test]
    procedure Create_Default_HasNoCustomButtons;
  end;

  [TestFixture]
  TStyledTaskDialogRadioTests = class
  public
    /// <summary>
    ///  Regression (D1). On the styled path RadioButton was only set inside the
    ///  RTL handler, which runs only when OnRadioButtonClicked is assigned: a
    ///  caller without the event read RadioButton = nil after Execute (the
    ///  native path always sets it).
    /// </summary>
    [Test]
    procedure RadioButtonClicked_WithoutEventHandler_SetsRadioButton;

    /// <summary>Guard: with the event assigned RadioButton is set as before.</summary>
    [Test]
    procedure RadioButtonClicked_WithEventHandler_SetsRadioButton;
  end;

implementation

type
  TRadioProbe = class
  public
    Count: Integer;
    procedure Clicked(Sender: TObject);
  end;

procedure TRadioProbe.Clicked(Sender: TObject);
begin
  Inc(Count);
end;

{ TStyledTaskDialogSmokeTests }

procedure TStyledTaskDialogSmokeTests.Create_Default_HasNoCustomButtons;
var
  LDialog: TStyledTaskDialog;
begin
  LDialog := TStyledTaskDialog.Create(nil);
  try
    Assert.AreEqual(0, LDialog.Buttons.Count, 'Buttons.Count');
  finally
    LDialog.Free;
  end;
end;

{ TStyledTaskDialogRadioTests }

procedure TStyledTaskDialogRadioTests.RadioButtonClicked_WithoutEventHandler_SetsRadioButton;
var
  LDialog: TStyledTaskDialog;
  LFirst, LSecond: TTaskDialogRadioButtonItem;
begin
  LDialog := TStyledTaskDialog.Create(nil);
  try
    LFirst := LDialog.RadioButtons.Add as TTaskDialogRadioButtonItem;
    LFirst.Caption := 'One';
    LSecond := LDialog.RadioButtons.Add as TTaskDialogRadioButtonItem;
    LSecond.Caption := 'Two';
    Assert.IsFalse(Assigned(LDialog.OnRadioButtonClicked), 'precondition: no handler');
    // What the styled form does when the user picks the second radio
    LDialog.DoOnRadioButtonClicked(LSecond.ModalResult);
    Assert.IsNotNull(LDialog.RadioButton, 'RadioButton must be set without an event handler');
    Assert.AreSame(LSecond, LDialog.RadioButton, 'RadioButton must be the clicked item');
  finally
    LDialog.Free;
  end;
end;

procedure TStyledTaskDialogRadioTests.RadioButtonClicked_WithEventHandler_SetsRadioButton;
var
  LDialog: TStyledTaskDialog;
  LProbe: TRadioProbe;
  LItem: TTaskDialogRadioButtonItem;
begin
  LDialog := TStyledTaskDialog.Create(nil);
  LProbe := TRadioProbe.Create;
  try
    LItem := LDialog.RadioButtons.Add as TTaskDialogRadioButtonItem;
    LItem.Caption := 'One';
    LDialog.OnRadioButtonClicked := LProbe.Clicked;
    LDialog.DoOnRadioButtonClicked(LItem.ModalResult);
    Assert.AreEqual(1, LProbe.Count, 'event fired once');
    Assert.AreSame(LItem, LDialog.RadioButton, 'RadioButton set with the event handler');
  finally
    LProbe.Free;
    LDialog.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TStyledTaskDialogSmokeTests);
  TDUnitX.RegisterTestFixture(TStyledTaskDialogRadioTests);

end.
