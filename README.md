# Delphi StyledComponents [![License](https://img.shields.io/badge/License-Apache%202.0-yellowgreen.svg)](https://opensource.org/licenses/Apache-2.0)

## Delphi VCL Components (Button and TaskDialog) with Custom Graphic Styles

### Actual official "beta" version: 0.9.0

| Component | Description |
| - | - |
| ![OK_BUTTON_GRAPH_128.png](./Images/OK_GRAPH_BUTTON_128.png) | **TStyledGraphicButton** is a "pure" Graphic Button with Styles (eg. Bootstrap) with support of ImageList, Action and full configuration of five states: Normal, Hot, Focused, Down and Disabled. You can use it also into a TVirtualList component.|
| ![OK_BUTTON_128.png](./Images/OK_BUTTON_128.png) | **TStyledButton** inherits from TStyledGraphicButton and add supporto for focus and Tabstop to the button.|
| ![StyledTaskDialog_128.png](./Images/StyledTaskDialog_128.png) | **TStyledTaskDialog** is a special "TaskDialog" component with custom Button Captions and Icons. Using a special Form you can show a full customizable Dialog |

## Additional Components Required ##

Demos uses SVGIconImageList components, but you can use another Image/Icon support.

## Description ##

**TStyledGraphicButton**, **TStyledButton** and **TStyledTaskDialog** are designed to expand Button and Dialogs functionalities.

**Look at the Demo Folder:**

Notice: to build Demos you must first Download [SvgIconImageList Components](https://github.com/EtheaDev/SVGIconImageList)

**Demos\Delphi10_3+\StyledButtonsDemo**

A simple demo to show different "Styled Class" Buttons (similar to Bootstrap buttons) and outlined versions.

![StyledButtonDemo.jpg](./Images/StyledButtonDemo.jpg)

**Demos\StyledTaskDlgDemo**

A simple demo to show how to use StyledTaskDialog with custom icons, caption for buttons, etc.

Activating "Use Styled Dialog" you can use a special form to show complete custom Dialog, like in those pictures:

*Warning Dialog*

![StyledButtonDemo.jpg](./Images/WarningDialog.jpg)

*Error Dialog*

![StyledButtonDemo.jpg](./Images/ErrorDialog.jpg)

*Custom Dialog*

![StyledButtonDemo.jpg](./Images/CustomDialog.jpg)

**Demos\Delphi10_4+\StyledButtonInControlList**

A simple demo to show how to use StyledGraphicButton into a ControlList (only for D10.4+)

![StyledButtonInControlListDemo.jpg](./Images/StyledButtonInControlListDemo.jpg)


### Available from Delphi XE6 to Delphi 11 (32bit and 64bit platforms)

![Delphi 11 Alexandria Support](./Images/SupportingDelphi.jpg)

Related links: [embarcadero.com](https://www.embarcadero.com) - [learndelphi.org](https://learndelphi.org)

### RELEASE NOTES

03 Nov 2022: version 0.9.1 (VCL)
- Added TStyledGraphicButton

01 Nov 2022: version 0.9.0 (VCL)
- First "beta" version

Thanks to Paulo Alvis "PraButtonStyle component" for the inspiration of StyledButton.