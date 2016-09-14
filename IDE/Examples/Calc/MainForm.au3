#include<MainForm.afm>

$CurrVal = 0
$LastOP = ""
$ResetData = False

;*Summary: Performs the last action and sets the new
Func Calc($Num, $OP)
  Switch $LastOP
    Case "+"
      $CurrVal = $CurrVal + $Num
    Case "*"
      $CurrVal = $CurrVal * $Num
    Case "-"
      $CurrVal = $CurrVal - $Num
    Case "/"
      $CurrVal = $CurrVal / $Num
    Case Else
      $CurrVal = $Num
  EndSwitch
  $LastOP = $OP
  $ResetData = True
  GUICtrlSetData($IOEdit, $CurrVal)
EndFunc

;*Summary: Adds $Num to IOEdit
Func AddNum($Num)
  $Dat = GUICtrlRead($IOEdit)
  if ($Dat = "0") Or $ResetData then
    $Dat = $Num
    $ResetData = False
  Else
    $Dat = $Dat & $Num
  EndIf
  GUICtrlSetData($IOEdit, $Dat)
EndFunc

Func NumButton1Click()
  AddNum("1")
EndFunc

Func NumButton2Click()
  AddNum("2")
EndFunc

Func NumButton3Click()
  AddNum("3")
EndFunc

Func NumButton4Click()
  AddNum("4")
EndFunc

Func NumButton5Click()
  AddNum("5")
EndFunc

Func NumButton6Click()
  AddNum("6")
EndFunc

Func NumButton7Click()
  AddNum("7")
EndFunc

Func NumButton8Click()
  AddNum("8")
EndFunc

Func NumButton9Click()
  AddNum("9")
EndFunc

Func NumButton0Click()
  AddNum("0")
EndFunc

Func DotButtonClick()
  $Dat = GUICtrlRead($IOEdit)
  if Not StringInStr($Dat, ".") then
    $Dat = $Dat & "."
  EndIf
  GUICtrlSetData($IOEdit, $Dat)
EndFunc

Func MinusButtonClick()
  $Dat = GUICtrlRead($IOEdit)
  if Not StringInStr($Dat, "-") then
    $Dat = "-" & $Dat
  Else
    $Dat = StringMid($Dat, 2)
  EndIf
  GUICtrlSetData($IOEdit, $Dat)
EndFunc

Func ClearButtonClick()
  GUICtrlSetData($IOEdit, "0")
EndFunc

Func ClearAllButtonClick()
  ClearButtonClick()
  $CurrVal = 0
  $LastOP = ""
EndFunc

Func AddButtonClick()
  Calc(GUICtrlRead($IOEdit), "+")
EndFunc

Func SubButtonClick()
  Calc(GUICtrlRead($IOEdit), "-")
EndFunc

Func MulButtonClick()
  Calc(GUICtrlRead($IOEdit), "*")
EndFunc

Func DivButtonClick()
  Calc(GUICtrlRead($IOEdit), "/")
EndFunc

Func CalcButtonClick()
  Calc(GUICtrlRead($IOEdit), "")
EndFunc
