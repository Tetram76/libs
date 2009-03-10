object AnimatorForm: TAnimatorForm
  Left = 370
  Top = 120
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMinimize]
  ClientHeight = 421
  ClientWidth = 366
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poDesktopCenter
  Scaled = False
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 4
    Top = 40
    Width = 157
    Height = 357
    Shape = bsFrame
  end
  object Frame1Lbl: TLabel
    Left = 12
    Top = 119
    Width = 39
    Height = 13
    Caption = 'Frame #'
    Transparent = True
    Visible = False
  end
  object Frame2Lbl: TLabel
    Tag = 1
    Left = 12
    Top = 206
    Width = 39
    Height = 13
    Caption = 'Frame #'
    Transparent = True
    Visible = False
  end
  object Frame3Lbl: TLabel
    Tag = 2
    Left = 12
    Top = 292
    Width = 39
    Height = 13
    Caption = 'Frame #'
    Transparent = True
    Visible = False
  end
  object Frame4Lbl: TLabel
    Tag = 3
    Left = 12
    Top = 379
    Width = 39
    Height = 13
    Caption = 'Frame #'
    Transparent = True
    Visible = False
  end
  object No1: TLabel
    Left = 62
    Top = 119
    Width = 24
    Height = 13
    Alignment = taRightJustify
    Caption = '2000'
    Visible = False
  end
  object No2: TLabel
    Tag = 1
    Left = 62
    Top = 206
    Width = 24
    Height = 13
    Alignment = taRightJustify
    Caption = '2000'
    Visible = False
  end
  object No3: TLabel
    Tag = 2
    Left = 62
    Top = 292
    Width = 24
    Height = 13
    Alignment = taRightJustify
    Caption = '2000'
    Visible = False
  end
  object No4: TLabel
    Tag = 3
    Left = 62
    Top = 379
    Width = 24
    Height = 13
    Alignment = taRightJustify
    Caption = '2000'
    Visible = False
  end
  object Status: TLabel
    Left = 8
    Top = 403
    Width = 152
    Height = 13
    AutoSize = False
    Transparent = True
  end
  object JvSpeedbar: TJvSpeedBar
    Left = 0
    Top = 0
    Width = 366
    Height = 31
    ParentFont = True
    BoundLines = [blBottom]
    Options = [sbFlatBtns, sbGrayedBtns]
    BtnOffsetHorz = 3
    BtnOffsetVert = 3
    BtnWidth = 24
    BtnHeight = 23
    IniStorage = Storage
    Images = ToolbarImages
    BevelOuter = bvNone
    TabOrder = 4
    OnDblClick = SpeedBarDblClick
    InternalVer = 1
    object SpeedbarSection1: TJvSpeedBarSection
      Caption = 'File'
    end
    object SpeedbarSection2: TJvSpeedBarSection
      Caption = 'Edit'
    end
    object SpeedbarSection3: TJvSpeedBarSection
      Caption = 'Image'
    end
    object SpeedbarSection4: TJvSpeedBarSection
      Caption = 'Help'
    end
    object NewBtn: TJvSpeedItem
      Caption = 'New'
      Hint = 'New|'
      ImageIndex = 8
      Spacing = 1
      Left = 11
      Top = 3
      Visible = True
      OnClick = NewBtnClick
      SectionName = 'File'
    end
    object OpenBtn: TJvSpeedItem
      Caption = 'Open'
      Hint = 'Open|'
      ImageIndex = 9
      Spacing = 1
      Left = 35
      Top = 3
      Visible = True
      OnClick = OpenBtnClick
      SectionName = 'File'
    end
    object SaveBtn: TJvSpeedItem
      Caption = 'Save'
      Enabled = False
      Hint = 'Save|'
      ImageIndex = 12
      Spacing = 1
      Left = 59
      Top = 3
      Visible = True
      OnClick = SaveBtnClick
      SectionName = 'File'
    end
    object SaveAsBtn: TJvSpeedItem
      Caption = 'SaveAs'
      Enabled = False
      Hint = 'SaveAs|'
      ImageIndex = 13
      Spacing = 1
      Left = 83
      Top = 3
      Visible = True
      OnClick = SaveAsBtnClick
      SectionName = 'File'
    end
    object InsertBtn: TJvSpeedItem
      Caption = 'Insert'
      Hint = 'Insert|'
      ImageIndex = 5
      Spacing = 1
      Left = 107
      Top = 3
      Visible = True
      OnClick = InsertBtnClick
      SectionName = 'File'
    end
    object CutBtn: TJvSpeedItem
      Caption = 'Cut'
      Enabled = False
      Hint = 'Cut|'
      ImageIndex = 3
      Spacing = 1
      Left = 139
      Top = 3
      Visible = True
      OnClick = CutBtnClick
      SectionName = 'Edit'
    end
    object CopyBtn: TJvSpeedItem
      Caption = 'Copy'
      Enabled = False
      Hint = 'Copy|'
      ImageIndex = 2
      Spacing = 1
      Left = 163
      Top = 3
      Visible = True
      OnClick = CopyBtnClick
      SectionName = 'Edit'
    end
    object PasteBtn: TJvSpeedItem
      Caption = 'Paste'
      Enabled = False
      Hint = 'Paste|'
      ImageIndex = 10
      Spacing = 1
      Left = 187
      Top = 3
      Visible = True
      OnClick = PasteBtnClick
      SectionName = 'Edit'
    end
    object DeleteBtn: TJvSpeedItem
      Caption = 'Delete'
      Enabled = False
      Hint = 'Delete|'
      ImageIndex = 4
      Spacing = 1
      Left = 211
      Top = 3
      Visible = True
      OnClick = DeleteBtnClick
      SectionName = 'Edit'
    end
    object UpBtn: TJvSpeedItem
      Caption = 'Move Up'
      Enabled = False
      Hint = 'Move Up|'
      ImageIndex = 7
      Spacing = 1
      Left = 243
      Top = 3
      Visible = True
      OnClick = UpBtnClick
      SectionName = 'Image'
    end
    object DownBtn: TJvSpeedItem
      Caption = 'Move Down'
      Enabled = False
      Hint = 'Move Down|'
      ImageIndex = 6
      Spacing = 1
      Left = 267
      Top = 3
      Visible = True
      OnClick = DownBtnClick
      SectionName = 'Image'
    end
    object GrayscaleBtn: TJvSpeedItem
      Caption = 'Grayscale'
      Hint = 'Grayscale|'
      ImageIndex = 0
      Spacing = 1
      Left = 323
      Top = 3
      OnClick = GrayscaleBtnClick
      SectionName = 'Image'
    end
    object PreviewBtn: TJvSpeedItem
      Caption = 'Preview'
      Enabled = False
      Hint = 'Preview|'
      ImageIndex = 11
      Spacing = 1
      Left = 299
      Top = 3
      Visible = True
      OnClick = PreviewBtnClick
      SectionName = 'Image'
    end
    object AboutBtn: TJvSpeedItem
      Caption = 'About'
      Hint = 'About|'
      ImageIndex = 1
      Spacing = 1
      Left = 331
      Top = 3
      OnClick = AboutBtnClick
      SectionName = 'Help'
    end
  end
  object Pages: TPageControl
    Left = 168
    Top = 40
    Width = 191
    Height = 357
    ActivePage = OptionsTab
    TabOrder = 6
    object OptionsTab: TTabSheet
      Caption = 'Options'
      object Label1: TLabel
        Left = 16
        Top = 245
        Width = 101
        Height = 13
        Caption = 'Color Depth Method: '
        FocusControl = ColorDepthCombo
      end
      object ThumbnailsLabel: TJvLabel
        Left = 30
        Top = 27
        Width = 125
        Height = 26
        Caption = 'Thumbnails Reflect Image'#13#10'Position'
        FocusControl = ThumbnailsBox
        ShowFocus = True
        OnMouseDown = CheckLabelMouseDown
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        ImageIndex = 0
      end
      object AlwaysOnTopLabel: TJvLabel
        Left = 30
        Top = 99
        Width = 100
        Height = 26
        Caption = 'Main Dialog Window'#13#10'Always On Top'
        FocusControl = AlwaysOnTop
        ShowFocus = True
        OnMouseDown = CheckLabelMouseDown
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        ImageIndex = 0
      end
      object ColorDepthCombo: TComboBox
        Left = 16
        Top = 261
        Width = 153
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = ColorDepthComboChange
        Items.Strings = (
          'Frequency of use'
          'Quantizing'
          'Truncate 7x8x4'
          'Truncate 6x6x6'
          'Tripel ')
      end
      object ThumbnailsBox: TCheckBox
        Left = 13
        Top = 31
        Width = 17
        Height = 17
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = ThumbnailsBoxClick
      end
      object FlatBtns: TCheckBox
        Left = 13
        Top = 67
        Width = 148
        Height = 17
        Caption = 'Flat Toolbar Buttons '
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = FlatBtnsClick
      end
      object AlwaysOnTop: TCheckBox
        Left = 13
        Top = 103
        Width = 17
        Height = 17
        TabOrder = 3
        OnClick = AlwaysOnTopClick
      end
    end
    object AnimationTab: TTabSheet
      Caption = 'Animation'
      object Bevel6: TBevel
        Left = 106
        Top = 85
        Width = 69
        Height = 21
      end
      object Bevel3: TBevel
        Left = 106
        Top = 60
        Width = 69
        Height = 21
      end
      object Label2: TLabel
        Left = 7
        Top = 213
        Width = 90
        Height = 13
        Caption = '&Trailing Comment:  '
        FocusControl = TrailingComment
      end
      object Label4: TLabel
        Left = 7
        Top = 15
        Width = 86
        Height = 13
        Caption = 'Animation Width:  '
      end
      object Label5: TLabel
        Left = 7
        Top = 40
        Width = 89
        Height = 13
        Caption = 'Animation Height:  '
      end
      object Bevel1: TBevel
        Left = 106
        Top = 11
        Width = 69
        Height = 21
      end
      object Bevel2: TBevel
        Left = 106
        Top = 35
        Width = 69
        Height = 21
      end
      object ScreenW: TLabel
        Left = 111
        Top = 14
        Width = 58
        Height = 15
        AutoSize = False
        Caption = '0'
      end
      object ScreenH: TLabel
        Left = 111
        Top = 38
        Width = 58
        Height = 15
        AutoSize = False
        Caption = '0'
      end
      object Label6: TLabel
        Left = 7
        Top = 64
        Width = 69
        Height = 13
        Caption = 'Image Count:  '
      end
      object ImageCount: TLabel
        Left = 111
        Top = 63
        Width = 58
        Height = 15
        AutoSize = False
        Caption = '0'
      end
      object Label7: TLabel
        Left = 7
        Top = 114
        Width = 94
        Height = 13
        Caption = '&Background Color:  '
        FocusControl = BackColor
      end
      object Label14: TLabel
        Left = 7
        Top = 89
        Width = 64
        Height = 13
        Caption = 'GIF Version:  '
      end
      object GIFVersion: TLabel
        Left = 111
        Top = 88
        Width = 58
        Height = 15
        AutoSize = False
        Caption = '87a'
      end
      object LoopLbl: TJvLabel
        Left = 27
        Top = 167
        Width = 75
        Height = 14
        Caption = '&Repeat Count: '
        Enabled = False
        FocusControl = RepeatCnt
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        ImageIndex = 0
      end
      object TrailingComment: TMemo
        Left = 4
        Top = 229
        Width = 176
        Height = 96
        ScrollBars = ssVertical
        TabOrder = 1
        OnChange = CommentChange
      end
      object BackColor: TJvComboEdit
        Left = 106
        Top = 110
        Width = 69
        Height = 21
        ButtonWidth = 17
        Color = clBlack
        DirectInput = False
        ImageKind = ikEllipsis
        TabOrder = 0
        OnButtonClick = BackColorButtonClick
      end
      object RepeatCntBtn: TUpDown
        Left = 161
        Top = 163
        Width = 15
        Height = 21
        Associate = RepeatCnt
        Enabled = False
        Max = 30000
        TabOrder = 2
      end
      object RepeatCnt: TEdit
        Left = 106
        Top = 163
        Width = 55
        Height = 21
        CharCase = ecUpperCase
        Enabled = False
        MaxLength = 5
        TabOrder = 3
        Text = '0'
        OnChange = LoopChange
      end
      object LoopBox: TCheckBox
        Left = 4
        Top = 140
        Width = 97
        Height = 17
        Caption = '&Looping'
        TabOrder = 4
        OnClick = LoopChange
      end
      object RepeatForever: TCheckBox
        Left = 27
        Top = 191
        Width = 97
        Height = 17
        Caption = 'Repeat &Forever'
        Enabled = False
        TabOrder = 5
        OnClick = LoopChange
      end
    end
    object ImageTab: TTabSheet
      Caption = 'Image'
      object Bevel4: TBevel
        Left = 108
        Top = 7
        Width = 69
        Height = 21
      end
      object Label3: TLabel
        Left = 6
        Top = 238
        Width = 85
        Height = 13
        Caption = 'Image Comment:  '
        FocusControl = FrameComment
      end
      object Label8: TLabel
        Left = 6
        Top = 11
        Width = 69
        Height = 13
        Caption = 'Image Width:  '
      end
      object FrameW: TLabel
        Left = 113
        Top = 10
        Width = 58
        Height = 15
        AutoSize = False
        Caption = '0'
      end
      object Bevel5: TBevel
        Left = 108
        Top = 32
        Width = 69
        Height = 21
      end
      object FrameH: TLabel
        Left = 113
        Top = 35
        Width = 58
        Height = 15
        AutoSize = False
        Caption = '0'
      end
      object Label11: TLabel
        Left = 6
        Top = 37
        Width = 72
        Height = 13
        Caption = 'Image Height:  '
      end
      object Label9: TLabel
        Left = 6
        Top = 64
        Width = 27
        Height = 13
        Caption = 'Left:  '
        FocusControl = ImageLeft
      end
      object Label10: TLabel
        Left = 6
        Top = 92
        Width = 28
        Height = 13
        Caption = 'Top:  '
        FocusControl = ImageTop
      end
      object Label12: TLabel
        Left = 6
        Top = 120
        Width = 95
        Height = 13
        Caption = 'Duration (1/100 s):  '
        FocusControl = DelayTime
      end
      object Label13: TLabel
        Left = 6
        Top = 144
        Width = 85
        Height = 13
        Caption = 'Undraw Method:  '
        FocusControl = DisposalCombo
      end
      object TransColorLabel: TJvLabel
        Left = 6
        Top = 216
        Width = 96
        Height = 14
        Caption = 'Transparent Color:  '
        Enabled = False
        FocusControl = TransColor
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        ImageIndex = 0
      end
      object FrameComment: TMemo
        Left = 1
        Top = 254
        Width = 180
        Height = 71
        ScrollBars = ssVertical
        TabOrder = 9
        OnChange = CommentChange
      end
      object ImageLeft: TEdit
        Left = 108
        Top = 60
        Width = 55
        Height = 21
        CharCase = ecUpperCase
        MaxLength = 5
        TabOrder = 0
        Text = '0'
        OnChange = TopLeftChange
      end
      object ImageLeftBtn: TUpDown
        Left = 163
        Top = 60
        Width = 15
        Height = 21
        Associate = ImageLeft
        Max = 32767
        TabOrder = 1
      end
      object ImageTop: TEdit
        Left = 108
        Top = 88
        Width = 55
        Height = 21
        CharCase = ecUpperCase
        MaxLength = 5
        TabOrder = 2
        Text = '0'
        OnChange = TopLeftChange
      end
      object ImageTopBtn: TUpDown
        Left = 163
        Top = 88
        Width = 15
        Height = 21
        Associate = ImageTop
        Max = 32767
        TabOrder = 3
      end
      object DelayTime: TEdit
        Left = 108
        Top = 116
        Width = 55
        Height = 21
        CharCase = ecUpperCase
        MaxLength = 10
        TabOrder = 4
        Text = '0'
        OnChange = DelayTimeChange
      end
      object DelayTimeBtn: TUpDown
        Left = 163
        Top = 116
        Width = 15
        Height = 21
        Associate = DelayTime
        Max = 32767
        TabOrder = 5
      end
      object DisposalCombo: TComboBox
        Left = 6
        Top = 160
        Width = 171
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 6
        OnChange = DisposalComboChange
        Items.Strings = (
          'Undefined'
          'Leave'
          'Restore Background'
          'Restore Previous')
      end
      object TransColor: TJvComboEdit
        Left = 124
        Top = 212
        Width = 52
        Height = 21
        ButtonWidth = 17
        Color = clBlack
        DirectInput = False
        ImageKind = ikEllipsis
        TabOrder = 8
        OnButtonClick = TransColorButtonClick
      end
      object TransBox: TCheckBox
        Left = 6
        Top = 191
        Width = 99
        Height = 17
        Caption = 'Transparency  '
        TabOrder = 7
        OnClick = TransBoxClick
      end
    end
  end
  object Progress: TProgressBar
    Left = 169
    Top = 403
    Width = 189
    Height = 13
    TabOrder = 7
  end
  object ImageScroll: TScrollBar
    Left = 139
    Top = 46
    Width = 16
    Height = 345
    Enabled = False
    Kind = sbVertical
    LargeChange = 4
    Max = 1
    PageSize = 0
    TabOrder = 5
    OnChange = ImageScrollChange
  end
  object Frame1: TPanel
    Left = 12
    Top = 48
    Width = 121
    Height = 69
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 0
    object PaintBox1: TPaintBox
      Left = 1
      Top = 1
      Width = 119
      Height = 67
      Align = alClient
      OnMouseDown = PaintBoxMouseDown
      OnPaint = PaintBoxPaint
    end
  end
  object Frame2: TPanel
    Tag = 1
    Left = 12
    Top = 135
    Width = 121
    Height = 69
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 1
    object PaintBox2: TPaintBox
      Tag = 1
      Left = 1
      Top = 1
      Width = 119
      Height = 67
      Align = alClient
      OnMouseDown = PaintBoxMouseDown
      OnPaint = PaintBoxPaint
    end
  end
  object Frame3: TPanel
    Tag = 2
    Left = 12
    Top = 222
    Width = 121
    Height = 69
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 2
    object PaintBox3: TPaintBox
      Tag = 2
      Left = 1
      Top = 1
      Width = 119
      Height = 67
      Align = alClient
      OnMouseDown = PaintBoxMouseDown
      OnPaint = PaintBoxPaint
    end
  end
  object Frame4: TPanel
    Tag = 3
    Left = 12
    Top = 309
    Width = 121
    Height = 69
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 3
    object PaintBox4: TPaintBox
      Tag = 3
      Left = 1
      Top = 1
      Width = 119
      Height = 67
      Align = alClient
      OnMouseDown = PaintBoxMouseDown
      OnPaint = PaintBoxPaint
    end
  end
  object Storage: TJvFormStorage
    AppStoragePath = '%FORM_NAME%\'
    PreventResize = True
    StoredProps.Strings = (
      'ThumbnailsBox.Checked'
      'FlatBtns.Checked'
      'AlwaysOnTop.Checked')
    StoredValues = <>
    Left = 8
    Top = 32
  end
  object ToolbarImages: TImageList
    DrawingStyle = dsTransparent
    Height = 15
    ImageType = itMask
    Left = 36
    Top = 32
    Bitmap = {
      494C01010E001300040010000F00FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004B0000000100200000000000004B
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
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000008080000000
      0000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000000
      0000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000808000008080000000
      0000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000008080000080800000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000008080000000000000808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000008080000080800000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000008080000000000000808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000008080000000000000808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000008080000000000000808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000000000000000000000000000000000000000000000
      0000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000808000000000000000000000000000000000000000
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
      0000FF00FF00FF00FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FF00FF00000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000FF00FF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000000
      0000000000000000000000000000000000000000000000808000808080000080
      8000808080000080800000000000FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000080808000C0C0C000C0C0C0008080
      80000000000000000000FF00FF00FF00FF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000000000000000000000000000000000000000000080808000008080008080
      8000008080008080800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000080808000C0C0C000C0C0C000FFFF00008080
      80008080800000000000FF00FF00FF00FF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF000000
      0000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000808000808080000080
      8000808080000080800000000000FFFFFF00FF000000FF000000FF000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000C0C0C00000000000FF00FF00FF00FF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF00000000000080800000808000008080000080800000808000008080000080
      8000008080000080800000000000000000000000000080808000008080008080
      8000008080008080800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000C0C0C000FFFF0000C0C0C000C0C0C0008080
      8000C0C0C00000000000FF00FF00FF00FF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000808000808080000080
      8000808080000080800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000080808000FFFF0000FFFF0000C0C0C0008080
      80008080800000000000FF00FF00FF00FF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000080808000008080008080
      8000008080008080800000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000080808000C0C0C000C0C0C0008080
      800000000000FF00FF00FF00FF00FF00FF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000808000808080000080
      8000808080000080800080808000008080008080800000808000808080000080
      80008080800000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000008080000000
      0000000000000000000000000000000000000000000000000000000000008080
      80000080800000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000808000808080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000000000000080
      80008080800000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000008080008080
      80000000000000FFFF00000000000000000000FFFF0000000000008080008080
      80000080800000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000C0C0C00000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000800000008000000080000000800000008000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000008000000080000000800000008000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000008000000080000000800000008000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000080000000800000008000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
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
      000000000000FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C0000000000000000000C0C0C00000000000008080000000
      0000000000000000000000000000FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C000FFFF
      FF00FFFFFF00FFFFFF000080800000808000000000000000000000FFFF000080
      8000000000000080800000FFFF00000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C0000000
      000000000000C0C0C0000000000000FFFF000080800000000000FFFFFF000080
      80000080800000FFFF0000000000FF00FF000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C0000000
      00000000000000000000008080000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C0000000
      000000000000000000000080800000FFFF00FFFFFF0000000000000000000000
      0000FFFFFF0000FFFF0000808000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C0000000
      0000000000000080800000000000FFFFFF000000000000000000FFFFFF000000
      0000000000000080800000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00FF000000FF000000FF000000FF00
      0000FF000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C000FFFF
      FF00FFFFFF00FFFFFF00000000000080800000FFFF0000000000FFFFFF000000
      00000080800000FFFF0000000000FF00FF000000000000000000000000000000
      0000000000000000000000000000800000008000000080000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C0000000
      000000000000C0C0C0000080800000FFFF000000000000000000FFFFFF000000
      0000000000000080800000000000FF00FF000000000000000000000000000000
      0000000000000000000000000000000000008000000080000000800000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00FF000000FF000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C0000000
      000000000000FFFFFF000080800000808000C0C0C0000000000000000000C0C0
      C00000000000FF00FF00FF00FF00FF00FF000000000000000000000000008000
      0000800000008000000000000000000000000000000080000000800000008000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C0000000
      00000000000000000000C0C0C000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0
      C00000000000FF00FF00FF00FF00FF00FF000000000000000000000000008000
      0000800000008000000000000000000000000000000080000000800000008000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C0000000
      0000000000000000000000000000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0
      C00000000000FF00FF00FF00FF00FF00FF000000000000000000000000008000
      0000800000008000000000000000000000000000000080000000800000008000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C00000000000FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000800000008000000080000000800000008000000080000000800000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C00000000000FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000400000004B0000000100010000000000580200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FFFFC00F00000000C001808F000000008031808300000000
      80318003000000008031800300000000800180000000000080018F8000000000
      80018F80000000008FF18F80000000008FF18FA0000000008FF1800000000000
      8FF1E3E8000000008FF1E000000000008FF5F8FA000000008001F80000000000
      FFFFFFFFFFFFFFF3C007FFFFFC018017C007001F800180FEC007000F0001810C
      C007000700018204C007000300018204C007000100018204C007000100038204
      C007001F00038108C007001F000380F0C007001F00038010C00F8FF1000380F0
      C01FFFF9000380A0C03FFF75800780C0FFFFFF8FF87FFF80FFFFFFFFFFFFFFFF
      FFFB8007FFFFFFFFEFFF8007FEFFF83FC7F78001FC7FF83FC7EF8001F83FF83F
      E3CF8001F01FF83FF19F8007E00F8003F83F8007C007C007FC7F800F8003E00F
      F83F800FF83FF01FF1BF800FF83FF83FC3CF801FF83FFC7F87E7803FF83FFEFF
      9FF3807FFFFFFFFFFFFFFFFFFFFFFFFFFF80FFFFFFFFFFFF8000FFFFFFFFF9FF
      8000FC7FFC01F6CF9000FC7FFC01F6B79000FFFFFC01F6B79000FC7F0001F8B7
      9840FC7F0001FE8F8040FE3F0001FE3F9040FF1F0001FF7F9060E38F0003FE3F
      9000E38F0007FEBF9E00E38F000FFC9F8000F01F00FFFDDF8000F83F01FFFDDF
      FFF0FFFF03FFFDDF}
  end
end
