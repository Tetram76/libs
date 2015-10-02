object TMTimeLineMainForm: TTMTimeLineMainForm
  Left = 261
  Top = 138
  AutoScroll = False
  Caption = 'Team Manager Timeline demo'
  ClientHeight = 550
  ClientWidth = 710
  Color = clBtnFace
  Constraints.MinHeight = 376
  Constraints.MinWidth = 710
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 147
    Width = 710
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 150
    Width = 710
    Height = 381
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    TabStop = True
    object Label6: TLabel
      Left = 0
      Top = 0
      Width = 710
      Height = 13
      Align = alTop
      Caption = 'Images:'
    end
    object Label7: TLabel
      Left = 472
      Top = 256
      Width = 121
      Height = 13
      Caption = 'Keyboard navigation:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label18: TLabel
      Left = 496
      Top = 274
      Width = 159
      Height = 26
      Caption = 'Left/Right arrow:'#13#10#9'move Small Change days'
    end
    object Label19: TLabel
      Left = 496
      Top = 306
      Width = 161
      Height = 26
      Caption = 'Ctrl+Left/Right arrow:'#13#10#9'move Large Change days'
    end
    object Label20: TLabel
      Left = 496
      Top = 338
      Width = 152
      Height = 26
      Caption = 'Shift+Left/Right arrow:'#13#10#9'move selection one day'
    end
    object lvImages: TListView
      Left = 0
      Top = 13
      Width = 710
      Height = 56
      Hint = 'The images available for the control'
      Align = alTop
      Columns = <>
      IconOptions.Arrangement = iaLeft
      LargeImages = il16
      ReadOnly = True
      PopupMenu = popTimeLine
      SmallImages = il16
      TabOrder = 0
      OnSelectItem = lvImagesSelectItem
    end
    object gbDates: TGroupBox
      Left = 464
      Top = 80
      Width = 241
      Height = 161
      Caption = ' Dates: '
      TabOrder = 3
      object Label2: TLabel
        Left = 8
        Top = 16
        Width = 78
        Height = 13
        Caption = 'First visible date:'
      end
      object Label3: TLabel
        Left = 8
        Top = 64
        Width = 69
        Height = 13
        Caption = 'Selected date:'
      end
      object Label4: TLabel
        Left = 136
        Top = 16
        Width = 53
        Height = 13
        Caption = 'Add image:'
      end
      object Label5: TLabel
        Left = 136
        Top = 64
        Width = 36
        Height = 13
        Caption = 'at date:'
      end
      object dtpFirstDate: TDateTimePicker
        Left = 8
        Top = 32
        Width = 121
        Height = 21
        Hint = 'Sets the first displayed date'
        CalAlignment = dtaLeft
        Date = 36854.4251693056
        Time = 36854.4251693056
        DateFormat = dfShort
        DateMode = dmComboBox
        Kind = dtkDate
        ParseInput = False
        TabOrder = 0
        OnChange = dtpFirstDateChange
      end
      object dtpSelDate: TDateTimePicker
        Left = 8
        Top = 80
        Width = 121
        Height = 21
        Hint = 'Sets the selected day'
        CalAlignment = dtaLeft
        Date = 36854.4251693056
        Time = 36854.4251693056
        DateFormat = dfShort
        DateMode = dmComboBox
        Kind = dtkDate
        ParseInput = False
        TabOrder = 1
        OnChange = dtpSelDateChange
      end
      object edImageNo: TEdit
        Left = 136
        Top = 32
        Width = 81
        Height = 21
        Hint = 
          'Sets the imageindex to insert at the current date'#13#10'(use -1 to re' +
          'move image)'
        TabOrder = 2
        Text = '-1'
      end
      object udImageNo: TUpDown
        Left = 217
        Top = 32
        Width = 15
        Height = 21
        Associate = edImageNo
        Min = -1
        Position = -1
        TabOrder = 3
        Wrap = False
      end
      object dtpImageDate: TDateTimePicker
        Left = 136
        Top = 80
        Width = 96
        Height = 21
        Hint = 'Sets the date to insert  / delete an image'
        CalAlignment = dtaLeft
        Date = 36854.5870759491
        Time = 36854.5870759491
        DateFormat = dfShort
        DateMode = dmComboBox
        Kind = dtkDate
        ParseInput = False
        TabOrder = 4
      end
      object btnAdd: TButton
        Left = 157
        Top = 120
        Width = 75
        Height = 25
        Hint = 'Adds an image to the chosen date'
        Caption = '&Add'
        TabOrder = 5
        OnClick = btnAddClick
      end
    end
    object gbAppearance: TGroupBox
      Left = 200
      Top = 80
      Width = 257
      Height = 161
      Caption = ' Appearance: '
      TabOrder = 2
      object Label13: TLabel
        Left = 115
        Top = 48
        Width = 102
        Height = 13
        Caption = 'Assigned object style:'
      end
      object chkReadOnly: TCheckBox
        Left = 8
        Top = 16
        Width = 73
        Height = 17
        Hint = 'Makes the control read-only'
        Caption = 'Read-only'
        TabOrder = 0
        OnClick = chkReadOnlyClick
      end
      object chkFlat: TCheckBox
        Left = 8
        Top = 32
        Width = 41
        Height = 17
        Hint = 'Makes the control flat'
        Caption = 'Flat'
        TabOrder = 1
        OnClick = chkFlatClick
      end
      object chkRClick: TCheckBox
        Left = 8
        Top = 48
        Width = 105
        Height = 17
        Hint = 'Moves the selection on right-click too'
        Caption = 'Right click select'
        TabOrder = 2
        OnClick = chkRClickClick
      end
      object chkEnabled: TCheckBox
        Left = 8
        Top = 64
        Width = 97
        Height = 17
        Hint = 'Enables the control'
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = chkEnabledClick
      end
      object chkShowToday: TCheckBox
        Left = 8
        Top = 81
        Width = 81
        Height = 17
        Caption = 'Show today'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = chkShowTodayClick
      end
      object chkShowWeeks: TCheckBox
        Left = 8
        Top = 114
        Width = 81
        Height = 17
        Caption = 'Show weeks'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = chkShowWeeksClick
      end
      object chkShowMonths: TCheckBox
        Left = 8
        Top = 130
        Width = 89
        Height = 17
        Caption = 'Show months'
        Checked = True
        State = cbChecked
        TabOrder = 7
        OnClick = chkShowMonthsClick
      end
      object lbObjFontStyle: TCheckListBox
        Left = 115
        Top = 64
        Width = 126
        Height = 64
        Hint = 
          'Sets the font to use for day items that'#13#10'have a non-nil Objects ' +
          'item'
        OnClickCheck = lbObjFontStyleClickCheck
        ItemHeight = 13
        Items.Strings = (
          'Bold'
          'Italic'
          'Underline'
          'Strikeout')
        TabOrder = 8
      end
      object chkShowTodayIcon: TCheckBox
        Left = 8
        Top = 97
        Width = 107
        Height = 17
        Caption = 'Show today icon'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = chkShowTodayIconClick
      end
    end
    object gbMisc: TGroupBox
      Left = 248
      Top = 248
      Width = 209
      Height = 129
      Caption = ' Misc. '
      TabOrder = 5
      object btnLoad: TButton
        Left = 14
        Top = 24
        Width = 110
        Height = 25
        Hint = 'Loads a previously saved timeline file'
        Caption = 'Load from file...'
        TabOrder = 0
        OnClick = btnLoadClick
      end
      object btnSave: TButton
        Left = 14
        Top = 56
        Width = 110
        Height = 25
        Hint = 'Saves the current content to a timeline file'
        Caption = 'Save to file...'
        TabOrder = 1
        OnClick = btnSaveClick
      end
    end
    object gbFonts: TGroupBox
      Left = 8
      Top = 248
      Width = 233
      Height = 129
      Caption = ' Fonts and colors: '
      TabOrder = 4
      object btnFont: TButton
        Left = 14
        Top = 56
        Width = 90
        Height = 25
        Hint = 'Sets the font for days'
        Caption = 'Font...'
        TabOrder = 1
        OnClick = btnFontClick
      end
      object btnColor: TButton
        Left = 14
        Top = 24
        Width = 90
        Height = 25
        Hint = 'Sets the background color'
        Caption = 'Color...'
        TabOrder = 0
        OnClick = btnColorClick
      end
      object btnMonthFont: TButton
        Left = 14
        Top = 88
        Width = 90
        Height = 25
        Hint = 'Sets the font for months and years'
        Caption = 'Month font...'
        TabOrder = 2
        OnClick = btnMonthFontClick
      end
      object btnTodayColor: TButton
        Left = 118
        Top = 56
        Width = 90
        Height = 25
        Hint = 'Sets the background color for the today item'
        Caption = 'Today color...'
        TabOrder = 4
        OnClick = btnTodayColorClick
      end
      object btnLineColor: TButton
        Left = 118
        Top = 24
        Width = 90
        Height = 25
        Caption = 'Line color...'
        TabOrder = 3
        OnClick = btnLineColorClick
      end
      object btnPenColor: TButton
        Left = 118
        Top = 88
        Width = 90
        Height = 25
        Hint = 'Sets the color of the selection frame'
        Caption = 'Selection color...'
        TabOrder = 5
        OnClick = btnPenColorClick
      end
    end
    object gbWidths: TGroupBox
      Left = 8
      Top = 80
      Width = 185
      Height = 161
      Caption = ' Widths and sizes: '
      TabOrder = 1
      object Label1: TLabel
        Left = 32
        Top = 77
        Width = 50
        Height = 13
        Caption = 'Day width:'
      end
      object Label8: TLabel
        Left = 14
        Top = 101
        Width = 68
        Height = 13
        Caption = 'Sel. line width:'
      end
      object Label9: TLabel
        Left = 15
        Top = 27
        Width = 67
        Height = 13
        Caption = 'Small change:'
      end
      object Label10: TLabel
        Left = 13
        Top = 52
        Width = 69
        Height = 13
        Caption = 'Large change:'
      end
      object Label12: TLabel
        Left = 20
        Top = 126
        Width = 62
        Height = 13
        Caption = 'Button width:'
      end
      object Label11: TLabel
        Left = 140
        Top = 28
        Width = 22
        Height = 13
        Caption = 'days'
      end
      object Label14: TLabel
        Left = 140
        Top = 53
        Width = 22
        Height = 13
        Caption = 'days'
      end
      object Label15: TLabel
        Left = 140
        Top = 77
        Width = 26
        Height = 13
        Caption = 'pixels'
      end
      object Label16: TLabel
        Left = 140
        Top = 102
        Width = 26
        Height = 13
        Caption = 'pixels'
      end
      object Label17: TLabel
        Left = 140
        Top = 126
        Width = 26
        Height = 13
        Caption = 'pixels'
      end
      object edDayWidth: TEdit
        Left = 88
        Top = 73
        Width = 33
        Height = 21
        Hint = 'Sets the width in pixels of a single day'
        TabOrder = 4
        Text = '19'
      end
      object udDayWidth: TUpDown
        Left = 121
        Top = 73
        Width = 15
        Height = 21
        Associate = edDayWidth
        Min = 0
        Max = 32000
        Position = 19
        TabOrder = 5
        Wrap = False
        OnClick = udDayWidthClick
      end
      object edPenWidth: TEdit
        Left = 89
        Top = 98
        Width = 32
        Height = 21
        Hint = 'Sets the width of the selection frame'
        TabOrder = 6
        Text = '2'
      end
      object udPenWidth: TUpDown
        Left = 121
        Top = 98
        Width = 15
        Height = 21
        Associate = edPenWidth
        Min = 0
        Position = 2
        TabOrder = 7
        Wrap = False
        OnClick = udPenWidthClick
      end
      object edScrollSmall: TEdit
        Left = 88
        Top = 24
        Width = 33
        Height = 21
        Hint = 
          'Sets the number of days to scroll when clicking the buttons '#13#10'or' +
          ' when using the arrow-keys'
        TabOrder = 0
        Text = '7'
      end
      object udScrollSmall: TUpDown
        Left = 121
        Top = 24
        Width = 15
        Height = 21
        Associate = edScrollSmall
        Min = 1
        Max = 365
        Position = 7
        TabOrder = 1
        Wrap = False
        OnClick = udScrollSmallClick
      end
      object edScrollLarge: TEdit
        Left = 88
        Top = 49
        Width = 33
        Height = 21
        Hint = 
          'Sets the number of days to scroll when Ctrl + clicking the butto' +
          'ns '#13#10'or when using Ctrl+left arrow or Ctrl+right arrow'
        TabOrder = 2
        Text = '30'
      end
      object udScrollLarge: TUpDown
        Left = 121
        Top = 49
        Width = 15
        Height = 21
        Associate = edScrollLarge
        Min = 1
        Max = 365
        Position = 30
        TabOrder = 3
        Wrap = False
        OnClick = udScrollLargeClick
      end
      object edButtonWidth: TEdit
        Left = 88
        Top = 122
        Width = 33
        Height = 21
        Hint = 'Sets the width of the scrollbuttons'
        TabOrder = 8
        Text = '12'
      end
      object udButtonWidth: TUpDown
        Left = 121
        Top = 122
        Width = 15
        Height = 21
        Associate = edButtonWidth
        Min = 0
        Max = 35
        Position = 12
        TabOrder = 9
        Wrap = False
        OnClick = udButtonWidthClick
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 531
    Width = 710
    Height = 19
    Hint = 'Displays info about the control'
    Panels = <
      item
        Width = 300
      end
      item
        Width = 50
      end>
    SimplePanel = False
    OnResize = StatusBarResize
  end
  object popTimeLine: TPopupMenu
    Left = 240
    Top = 43
    object mnuEditMemo: TMenuItem
      Caption = 'Edit memo...'
      Default = True
      OnClick = mnuEditMemoClick
    end
    object mnuInsertImage: TMenuItem
      Caption = 'Insert image'
      ShortCut = 45
      OnClick = mnuInsertImageClick
    end
    object mnuRemoveImage: TMenuItem
      Caption = 'Remove image'
      ShortCut = 46
      OnClick = mnuRemoveImageClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuToday: TMenuItem
      Caption = 'Go to today'
      ShortCut = 116
      OnClick = mnuTodayClick
    end
    object mnuGotoDate: TMenuItem
      Caption = 'Go to selected day'
      ShortCut = 118
      OnClick = mnuGotoDateClick
    end
  end
  object il16: TImageList
    Height = 18
    Width = 14
    Left = 560
    Top = 16
    Bitmap = {
      494C01010800090004000E001200FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000038000000360000000100100000000000A017
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007C007C007C
      007C007C00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000007C007C007C007C007C0000000000000000
      000000000000007C007C007C007C007C007C007C007C007C0000000000000000
      000000001042FF7FFF7FFF7FFF7FFF7F00000000000000000000000000000000
      1042FF7FFF7FFF7FFF7FFF7F00000000000000000000000000000000007C007C
      007CFF7F007C007C007C000000000000000000000000007C007C007C007C007C
      007C007C007C007C000000000000000000001042FF7FFF7FFF7FFF7FFF7FFF7F
      FF7F0000000000000000000000001042FF7FFF7FFF7F00000000FF7FFF7F0000
      00000000000000000000007C007C007CFF7FFF7FFF7F007C007C007C00000000
      00000000007C007CFF7FFF7F007C007C007CFF7FFF7F007C007C000000000000
      1042FF7FFF7FFF7F0002FF7FFF7FFF7FFF7FFF7F00000000000000001042FF7F
      FF7FFF7FFF7F00000000FF7FFF7FFF7F0000000000000000007C007C007C007C
      007CFF7F007C007C007C007C007C000000000000007C007C007CFF7FFF7F007C
      FF7FFF7F007C007C007C0000000000001042FF7FFF7F000200020002FF7FFF7F
      FF7FFF7F00000000000000001042FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F
      0000000000000000007C007C007C007C007C007C007C007C007C007C007C0000
      00000000007C007C007C007CFF7FFF7FFF7F007C007C007C007C000000000000
      1042FF7FFF7F0002FF7F00020002FF7FFF7FFF7F00000000000000001042FF7F
      FF7FFF7FFF7F00000000FF7FFF7FFF7F0000000000000000007C007C007C007C
      007CFF7F007C007C007C007C007C000000000000007C007C007CFF7FFF7F007C
      FF7FFF7F007C007C007C0000000000001042FF7FFF7FFF7FFF7FFF7F00020002
      FF7FFF7F00000000000000001042FF7FFF7F0000FF7FFF7F00000000FF7FFF7F
      0000000000000000007C007C007C007CFF7FFF7FFF7F007C007C007C007C0000
      00000000007C007CFF7FFF7F007C007C007CFF7FFF7F007C007C000000000000
      1042FF7FFF7FFF7FFF7FFF7FFF7F0002FF7FFF7F00000000000000001042FF7F
      FF7F0000FF7FFF7F00000000FF7FFF7F0000000000000000007C007C007C007C
      FF7FFF7FFF7F007C007C007C007C0000000000000000007C007C007C007C007C
      007C007C007C007C000000000000000000001042FF7FFF7FFF7FFF7FFF7FFF7F
      FF7F0000000000000000000000001042FF7FFF7F000000000000FF7FFF7F0000
      00000000000000000000007C007C007CFF7FFF7FFF7F007C007C007C00000000
      000000000000007C007C007C007C007C007C007C007C007C0000000000000000
      000000001042FF7FFF7FFF7FFF7FFF7F00000000000000000000000000000000
      1042FF7FFF7FFF7FFF7FFF7F00000000000000000000000000000000007C007C
      007C007C007C007C007C00000000000000000000000000000000007C007C007C
      007C007C00000000000000000000000000000000000010421042104210421042
      0000000000000000000000000000000000001042104210421042104200000000
      0000000000000000000000000000007C007C007C007C007C0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007C007C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000007C007C007C007C00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000007C007C007C007C007C
      007C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000007C007C007CFF7FFF7F007C007C007C000000000000000000000000
      000000000000007C007C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000007C007C007C007C007C007C007C
      007C007C007C0000000000000000000000000000007C007C007C007C00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      007C007C007C007C007CFF7FFF7F007C007C007C007C007C0000000000000000
      0000007C007C007C007C007C007C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000007C007C007C007C007C007CFF7FFF7F007C
      007C007C007C007C007C000000000000007C007C007CFF7FFF7F007C007C007C
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      007C007C007C007C007CFF7FFF7F007C007C007C007C007C000000000000007C
      007C007C007C007C007C007C007C007C007C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000007C007C007C007CFF7FFF7F007C
      007C007C007C000000000000007C007C007C007C007CFF7FFF7F007C007C007C
      007C007C00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000007C007C007CFF7FFF7F007C007C007C000000000000007C007C007C
      007C007C007CFF7FFF7F007C007C007C007C007C007C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000007C007C0000007C007C007C007C007C
      007C0000007C007C00000000007C007C007C007C007CFF7FFF7F007C007C007C
      007C007C00000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000007C
      007C007C007C0000007C007C007C007C0000007C007C007C007C00000000007C
      007C007C007CFF7FFF7F007C007C007C007C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000007C007C007C007C0000007C007C0000
      007C007C007C007C0000000000000000007C007C007CFF7FFF7F007C007C007C
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000007C007C007C007C00000000007C007C007C007C00000000000000000000
      0000007C007C007C007C007C007C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000007C007C007C007C007C007C
      007C007C00000000000000000000000000000000007C007C007C007C00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000007C007C007C007C007C007C0000000000000000000000000000
      000000000000007C007C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000007C007C007C007C
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000007C007C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000038000000360000000100010000000000B00100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00
      F07FC1FF07FC1F00C01F80FE03F80F00C01F007C01F00700800E003800E00300
      800E003800E00300800E003800E00300800E003800E00300800E003800E00300
      C01F007C01F00700C01F80FE03F80F00F07FC1FF07FC1F00FFFFFFFFFFFFFF00
      FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FCFFF3FFFFFFFF00F87FE1FFFFFFFF00
      F03FC0FFFFFFFF00E01F807FCFFFFF00C00F003F87FFFF008006001F03FF7F00
      0000000E01FE3F008006001C00FC1F00C00F003800780F00E01F807000300700
      9026409800780F000840210C00FC1F008486121E01FE3F00C30F0C3F03FF7F00
      E01F807F87FFFF00F03FC0FFCFFFFF00F87FE1FFFFFFFF00FCFFF3FFFFFFFF00
      00000000000000000000000000000000000000000000}
  end
end
