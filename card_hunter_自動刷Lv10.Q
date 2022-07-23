[General]
SyntaxVersion=2
BeginHotkey=111
BeginHotkeyMod=8
PauseHotkey=0
PauseHotkeyMod=0
StopHotkey=106
StopHotkeyMod=0
RunOnce=1
EnableWindow=
MacroID=3e639148-e35a-48e7-9507-2f675f32acc6
Description=autoFarm10
Enable=1
AutoRun=0
[Repeat]
Type=0
Number=1
[SetupUI]
Type=2
QUI=
[Relative]
SetupOCXFile=
[Comment]

[Script]
		//import configs
		
screenRatio = 1.5 //windows UI zoom rate
Const windowRect = "-7|-7|1288|688"
windowWidth = 1288 * screenRatio
windowHeight = 688 * screenRatio
//debugMode = true
Dim mageMovePointX(2),mageMovePointy(2)
mageMovePointX(0) = windowWidth * 0.4663
mageMovePointY(0) = windowHeight * 0.4418
mageMovePointX(1) = windowWidth * 0.4958
mageMovePointY(1) = windowHeight * 0.4748	
mageMovePointX(2) = windowWidth * 0.5274
mageMovePointY(2) = windowHeight * 0.5029
	

		//import configs

gameHwnd = Plugin.Window.Find("ApolloRuntimeContentWindow", "Card Hunter")
Dim imgPathes(100)
Set imgs = CreateObject("Scripting.Dictionary")
startGameTime = 0
startButtonX = 0
startButtonY = 0
startButtonX = 907 //should be updated during runtime
startButtonY = 963 //should be updated during runtime

randomX1 = 72 // testing....
randomY1 = 963 // testing....
randomX2 = 1034 // testing....
randomY3 = 341 // testing....

failedToFindImgCount = 0
findPicColorDiff = 10
findPicAccuracy = 0.9

lastFoundX = - 1 
lastFoundY = - 1 
foundAtSamePlace = 0

Sub somethingWentWrong(string)
	MsgBox "Something went wrong : " & string
	EndScript
End Sub

Sub setImgsPath
	const basicPath = "E:\Else\CardHunter\scripts\imgs\"
	imgPathes(0) = "greenTickBox"
	imgPathes(1) = "beginAdventure"
	imgPathes(2) = "armorLogo"
	imgPathes(3) = "shouldPlayTrait"
	imgPathes(4) = "searchHandicap"
	imgPathes(5) = "yourTurn"	
	imgPathes(6) = "wonAndOnward"
	imgPathes(7) = "openChest"
	imgPathes(8) = "chestLock"
	imgPathes(9) = "returnToMap"
	imgPathes(10) = "flagOnMap"
	imgPathes(11) = "walk"
	imgPathes(12) = "chooseALocation"
	imgPathes(13) = "flashAgony"
	imgPathes(14) = "checkFreeOnMap"	
	imgPathes(15) = "startAttackCheck1"
	imgPathes(16) = "startAttackCheck2"
	imgPathes(17) = "resignBattle"
	imgPathes(18) = "regroup"
	
	For i = 0 To 100
	    imgPath = imgPathes(i)
	    If (imgPath <> "") Then 
	        imgPath = basicPath & imgPathes(i) & ".bmp" 
			BMPArray = Plugin.BGCP2_02.LoadBMP(imgPath)
			If IsArray(BMPArray)=False
    			MsgBox "LoadBMP ERROR @ " & imgPath, 4096
        		EndScript
			End If
			imgs.Add imgPathes(i),BMPArray(0)
	    End If
	Next	
End Sub

Sub leaveTeamToReset()
	Call Plugin.Bkgnd.KeyPress(gameHwnd,113) //F2
	Delay 300	
	Call Plugin.Bkgnd.KeyPress(gameHwnd, 8) //Backescape
    Call Plugin.Bkgnd.SendString(gameHwnd,"team leave")
	Call Plugin.Bkgnd.KeyPress(gameHwnd, 13) //Enter	
	Delay 300
	Call Plugin.Bkgnd.KeyPress(gameHwnd, 112) //F1		
End Sub

Sub maxWindowAndSetAnimspeed
	Call Plugin.Window.Max(gameHwnd)
	TracePrint("window rect : " & Plugin.Window.GetWindowRect(gameHwnd) )
	Call Plugin.Bkgnd.KeyPress(gameHwnd,113) //F2
	Delay 300
	Call Plugin.Bkgnd.KeyPress(gameHwnd, 8) //Enter	
	Delay 300	
    Call Plugin.Bkgnd.SendString(gameHwnd,"animspeed 999")
	Call Plugin.Bkgnd.KeyPress(gameHwnd, 13) //Enter	
	Delay 300
	Call Plugin.Bkgnd.KeyPress(gameHwnd, 112) //F1		
End Sub

Sub mouseTo(x, y, toClick)
	If debugMode = true Then 
		TracePrint("mouse to : " & x & "  " & y)
	End If
	If toClick Then 
		Call Plugin.Bkgnd.LeftClick(gameHwnd, x / screenRatio, y / screenRatio)
		Call Plugin.Bkgnd.LeftClick(gameHwnd, x / screenRatio, y / screenRatio)
	End If	
End Sub

Function findImgAndClick(x1, y1, x2, y2, imgName, letPass, shouldClick)
	If imgs.Exists(imgName) = false Then 
		MsgBox "unknown img : " & imgName
		EndScript
	End If
	If (failedToFindImgCount > 20) Then 
		x1 = 0
		y1 = 0
		x2 = windowWidth
		y2 = windowHeight
	End If
	Call Plugin.BGCP2_02.FindPicture(gameHwnd, x1, y1, x2, y2, imgs.Item(imgName), findPicColorDiff, findPicAccuracy, x, y)
    If x >= 0 And y >= 0 Then 
       failedToFindImgCount = 0
	   findPicColorDiff = 10
	   findPicAccuracy = 0.9
	   
	   If x = lastFoundX and y = lastFoundY Then 
	   	foundAtSamePlace = foundAtSamePlace + 1
	   	If foundAtSamePlace > 50 Then 
	   	
     		If (imgName = "flashAgony") Then //misclick step card     		
 				Call resignAndHandleLose()
 				Call goIntoBattle()
 				Exit Function
     		End If
     	
	   		MsgBox "stuck at " & imgName
	   		ExitScript
	   	End If
	   Else 
	    lastFoundX = x : lastFoundY = y : foundAtSamePlace = 0	    
	   End If	   
	   
       Call mouseTo(x + 5, y + 3, shouldClick="Click")
       If (imgName = "beginAdventure") Then 
		startButtonX = x
		startButtonY = y
		TracePrint(" startButtonX : " & startButtonX)
		TracePrint(" startButtonY : " & startButtonY)
       End If
       findImgAndClick = x & "|" & y
    Else 
       failedToFindImgCount = failedToFindImgCount + 1
       If (failedToFIndImgCount = 10) Then 
    	   delay 1000   	
	       Call mouseTo(randomX1, randomY1, shouldClick = "Move")
	       Delay 1000
       End If        
       If (failedToFIndImgCount = 20) Then 
    	   delay 1000   	
	       Call mouseTo(randomX1, randomY1, shouldClick = "Move")
	       Delay 1000
       End If
	   findPicColorDiff = findPicColorDiff + 1
	   findPicAccuracy = findPicAccuracy - 0.01
       If (failedToFindImgCount > 25) Then 
	    TracePrint("img not found : " & x1 & " " & y1 & " " & x2 & " " & y2 & " " & imgName & " " & findPicColorDiff & " " & findPicAccuracy )
       	MsgBox "take a look..." & imgName  	
       	ExitScript
       End If
       If debugMode Then 
       	   TracePrint("img not found : " & x1 & " " & y1 & " " & x2 & " " & y2 & " " & imgName & " " & findPicColorDiff & " " & findPicAccuracy )
       End If
       If (letPass <> "LetPass") Then        	
	       Call	somethingWentWrong("findImg : " & imgName)
       End If
       findImgAndClick = ""
	End If	
End Function

Function playOneTypeOfCard(searchImg)
 	If debugMode Then TracePrint ("playAllTraits")
	shouldPlayTrait = findImgAndClick(0, windowHeight*3/4 ,  windowWidth  , windowHeight , searchImg,  "LetPass" , "Click")
	If (shouldPlayTrait = "") Then 
	 	playOneTypeOfCard = "DONE"
	 	Exit Function
	End If
	If (searchImg = "flashAgony") Then 
		Delay 2000
	End If
	Delay 1000
	playOneTypeOfCard = playOneTypeOfCard(searchImg)		
End Function

Function waitUntilIsMyTurn(imgName,clickPlace)
	isYourTurn = ""
	While isYourTurn = ""
		isYourTurn = findImgAndClick(windowWidth / 3, 0, windowWidth * 2 / 3, windowHeight / 3, imgName, "LetPass", "JustCheck")
		If (isYourTurn = "") Then 
			If (clickPlace <> "") Then 
				x = Split(clickPlace,"|")(0)
				y = Split(clickPlace, "|")(1)
    			Call mouseTo(x,y, true) 				
			End If
			If debugMode Then TracePrint ("waiting for your turn : " & imgName)
			delay 100
		End If 
	Wend	
End Function

Function checkIfCanStartAttack()
    checkIfCanStartAttack = true
    checkDownCockroaches = findImgAndClick(windowWidth/4, windowHeight/3,windowWidth/2, windowHeight*4/5, "startAttackCheck1", "LetPass", "JustCheck")
    If (checkDownCockroaches = "") Then 
    	 checkIfCanStartAttack = false
    Else 
         checkUpCockroaches = findImgAndClick(windowWidth / 2, windowHeight / 5, windowWidth * 4 / 5, windowHeight / 2, "startAttackCheck2", "LetPass", "JustCheck")
         If (checkUpCockroaches = "") Then checkIfCanStartAttack = false          
    End If
End Function

Function moveAllThreeMages()
	For index = 0 To 2		
		Call waitUntilIsMyTurn("yourTurn","")
		If debugMode Then TracePrint ("NOW PLAY")
		x1 = 0 : x2 = windowWidth * 0.4
		If (index = 1) Then 
	  		x1 = windowWidth * 0.4 : x2 = windowWidth * 0.62
		End If
		If (index = 2) Then 
	  		x1 = windowWidth * 0.62 : x2 = windowWidth
		End If
    	
    	clickMoveCard = findImgAndClick(x1, windowHeight * 4 / 7, x2, windowHeight, "walk", "LetPass", "Click")
    	If (clickMoveCard = "") Then 
    		Delay 1000
    		clickMoveCard = findImgAndClick(x1, windowHeight * 4 / 7, x2, windowHeight, "walk", "NotLetPass", "Click")
    	End If
    	
		Call waitUntilIsMyTurn("chooseALocation",clickMoveCard)
    	Call mouseTo(mageMovePointX(index), mageMovePointY(index), true)
    	
    	If (index = 1) Then 
    		Delay 600
    		checkResult = checkIfCanStartAttack()
    		If (checkResult) Then Exit Function    		
    	End If
    	
	Next
End Function

Sub returnToMap()	
	//try to click the Return to map button
	findResult = ""
	While findResult = ""
		Delay 100
		findResult = findImgAndClick(0,windowHeight*6/7,windowWidth/5, windowHeight, "returnToMap", "LetPass", "Click")
	Wend
	Call leaveTeamToReset()
	Delay 500
	//wait until party dismiss
	findResult = ""
	While findResult = ""
		Delay 100
		findResult = findImgAndClick(windowWidth/2,windowHeight/2,windowWidth, windowHeight, "checkFreeOnMap", "LetPass", "JustCheck")
	Wend
	Delay 2000
	//try to click the bright flag on map
	findResult = ""
	While findResult = ""
		Delay 100
		findResult = findImgAndClick(windowWidth / 3, windowHeight / 3, windowWidth * 2 / 3, windowHeight * 2 / 3, "flagOnMap", "LetPass", "Click")
	Wend
	//sometimes need a second click , don't really know why
	Delay 100
	x = Split(findResult,"|")(0)
	y = Split(findResult, "|")(1)
	Call mouseTo(x+5, y, true)
End Sub

Function openChestAndSellAll()
 	If debugMode Then TracePrint ("openChestAndSellAll")
	//click the start button, until open chest page
	findResult = ""
	While findResult = ""
		Delay 500 : Call mouseTo(startButtonX, startButtonY, true)
		//this y2 is veryimportant , because it has a scroll effect, cannot click too early
		findResult = findImgAndClick(windowWidth / 2, windowHeight / 5, windowWidth * 3 / 4, windowHeight*0.3711, "openChest", "LetPass", "Click")
	Wend		
	x = Split(findResult,"|")(0)
	y = Split(findResult, "|")(1)
	//click sell all, until chest is closed
	findResult = ""
	While findResult = ""
		Delay 1000 : Call mouseTo(x+100, y, true)
		findResult = findImgAndClick(windowWidth / 2, 0, windowWidth * 4 / 5, windowHeight / 2, "chestLock", "LetPass", "JustCheck")
	Wend
	Delay 100
	Call mouseTo(startButtonX, startButtonY, true)
	Call returnToMap()
End Function

Function checkIfHasWon()
    checkIfHasWon = false
    checkResult = findImgAndClick(windowWidth / 3, windowHeight / 3 , windowWidth * 2 / 3, windowHeight * 4 / 5, "wonAndOnward", "LetPass", "Click")
	If (checkResult <> "") Then 
	  checkIfHasWon = true
	  openChest = openChestAndSellAll()
	End If
End Function

Function countCards(searchImg, x1)
	find = findImgAndClick(x1, windowHeight*3/4 ,  windowWidth  , windowHeight , searchImg,  "LetPass" , "JustCheck")
    If (find = "") Then 
    	countCards = 0
    Else 
        countCards = 1
        nextX1 = CInt( Split(find, "|")(0) ) + 65
        If (nextX1 < windowWidth * 0.8188) Then 
        	countCards = countCards + countCards(searchImg,nextX1)
        End If    	
    End If    
End Function

Sub resignAndHandleLose()

	Call Plugin.Bkgnd.KeyPress(gameHwnd,27) //F2	
	//resign
	Delay 200
  	Call mouseTo(windowWidth * 0.4984, windowHeight * 0.3566, true)
  	
	//confirm
	Delay 200
  	Call mouseTo(windowWidth * 0.5492, windowHeight * 0.5484, true)
  	//regroup
  	
	findResult = ""
	While findResult = ""
		Delay 300
		findResult = findImgAndClick(windowWidth/3, windowHeight/2, windowWidth*2/3 , windowHeight, "regroup",  "LetPass" , "Click")
	Wend
	Delay 2000
	Call returnToMap()
		
End Sub

Sub playUntilEndGame()	
	Call playOneTypeOfCard("flashAgony")		
	hasWon = checkIfHasWon()
	If (hasWon = false) Then //the black flash scree might cause this
		Call playOneTypeOfCard("flashAgony")
		hasWon = checkIfHasWon()
		If (hasWon = false) Then 
			Call resignAndHandleLose()
		End If		
	End If    
    Call goIntoBattle()
End Sub

Sub goIntoBattle()
	startGameTime = startGameTime + 1
	TracePrint(" startGameTime : " & startGameTime)
	//try to click the green tick
	
	findResult = ""
	While findResult = ""
		Delay 500: findResult = findImgAndClick(windowWidth / 2, windowHeight / 5, windowWidth, windowHeight / 2, "greenTickBox", "LetPass", "Click")
	Wend
    	
	findResult = findImgAndClick(windowWidth / 2, windowHeight / 5, windowWidth, windowHeight / 2, "greenTickBox", "NotLetPass", "Click") : Delay 100
	If (startButtonX = 0) Then findResult = findImgAndClick(windowWidth * 1 / 3, windowHeight * 4 / 5, windowWidth * 2 / 3, windowHeight, "beginAdventure", "LetPass", "Click")
	If (startButtonX <> 0) Then 
		findResult = ""
		While findResult = ""
			//click the same place, until we are in the battle
			Call mouseTo(startButtonX, startButtonY, true) : Delay 500
			findResult = findImgAndClick(windowWidth * 6 / 7, windowHeight * 5 / 7, windowWidth , windowHeight, "armorLogo",  "LetPass" , "JustCheck")
		Wend
	End If
	
	Delay 500 : playingTrait = playOneTypeOfCard("searchHandicap") : Delay 500
	
 	count = countCards("flashAgony", 0)
 	If (count < 2) Then 
 		Call resignAndHandleLose()
 		Call goIntoBattle()
 		Exit Sub
 	End if
	
	Call moveAllThreeMages() : Delay 1000	
 	
 	Call playUntilEndGame()
	
End Sub

Sub startAutoFarmCockroach()
    Call setImgsPath()
    Call maxWindowAndSetAnimspeed()
    Call goIntoBattle()
End Sub

TracePrint("GO")
Call startAutoFarmCockroach()
