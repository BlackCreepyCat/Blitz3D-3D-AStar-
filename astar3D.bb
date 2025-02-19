; 4-direction only A* algo for hellfire
; by morduun, with the aid of turtle's tut

; F = 'best-fit' value, is calc'd by combining G + H
; G = movement value.  10 for plains, 20 for brush and forest, and 30 for hills.
;		normally you'd do other stuff for 8-directional movement, but since hellfire
;		only allows 4-directional, this is fine for my application.
; H = guesstimate of how long it takes to get from this square to the end.
;		the "manhattan" estimate is basically an assessment of G from the current position
;		to the target, without taking non-walkable areas into account.



; ported to 3D for Project Plasma2004 by jfk of CSP (visit www.melog.ch/dropper/ ;) ).
; Not shure if I did this correctly, but it is KINDA working...
; PLEASE NOTE: This Code uses huge amounts of Ram when a complex Path is required 
; (eg: multiple big S-shapes etc.)
; I am still trying to find a solution for this memory problem. Precalculation might be
; the way. There is still no diagonal G-cost correction, see function "getLowF()", so the
; resulting path is kinda ziczac sometimes. However, this could be used as a feature :)

; 4-directional neighbor check replaced by 3D priority chart, Manhatten replaced by 
; lookup-pythagoras.

Graphics3D 800, 600, 32, 2

Include "lists.bb"
SeedRnd MilliSecs()

Type tile
	Field x
	Field y
	Field z
	Field walkable
	Field parent.tile
	Field f
	Field g
	Field h
	Field myNode.node
End Type

Type waypoint
	Field x
	Field y
	Field z
	Field myNext.waypoint
End Type


Type coord
	Field x%
	Field y%
	Field z%
End Type

Global start.coord = New coord		
Global goal.coord = New coord		

Global open.list = newList()
Global closed.list = newList()

Global mapsize = 20
Global gridsize#=2.0


Dim map.tile(mapsize-1, mapsize-1, mapsize-1)

; neighbor check priority chart
Dim neigh(26,2)
neigh(0,0)=-1 :neigh(0,1)= 0 :neigh(0,2)= 0
neigh(1,0)= 1 :neigh(1,1)= 0 :neigh(1,2)= 0
neigh(2,0)= 0 :neigh(2,1)= 0 :neigh(2,2)=-1
neigh(3,0)= 0 :neigh(3,1)= 0 :neigh(3,2)= 1
neigh(4,0)= 0 :neigh(4,1)=-1 :neigh(4,2)= 0
neigh(5,0)= 0 :neigh(5,1)= 1 :neigh(5,2)= 0

neigh(6,0)= 1 :neigh(6,1)= 1 :neigh(6,2)= 0
neigh(7,0)=-1 :neigh(7,1)=-1 :neigh(7,2)= 0
neigh(8,0)=-1 :neigh(8,1)= 1 :neigh(8,2)= 0
neigh(9,0)= 1 :neigh(9,1)=-1 :neigh(9,2)= 0
neigh(10,0)= 1 :neigh(10,1)= 0 :neigh(10,2)= 1
neigh(11,0)=-1 :neigh(11,1)= 0 :neigh(11,2)=-1
neigh(12,0)=-1 :neigh(12,1)= 0 :neigh(12,2)= 1
neigh(13,0)= 1 :neigh(13,1)= 0 :neigh(13,2)=-1
neigh(14,0)= 0 :neigh(14,1)= 1 :neigh(14,2)= 1
neigh(15,0)= 0 :neigh(15,1)=-1 :neigh(15,2)=-1
neigh(16,0)= 0 :neigh(16,1)=-1 :neigh(16,2)= 1
neigh(17,0)= 0 :neigh(17,1)= 1 :neigh(17,2)=-1

neigh(18,0)= 1 :neigh(18,1)= 1 :neigh(18,2)= 1
neigh(19,0)= 1 :neigh(19,1)= 1 :neigh(19,2)=-1
neigh(20,0)= 1 :neigh(20,1)=-1 :neigh(20,2)=-1
neigh(21,0)=-1 :neigh(21,1)=-1 :neigh(21,2)=-1
neigh(22,0)=-1 :neigh(22,1)=-1 :neigh(22,2)= 1
neigh(23,0)=-1 :neigh(23,1)= 1 :neigh(23,2)= 1
neigh(24,0)=-1 :neigh(24,1)= 1 :neigh(24,2)=-1
neigh(25,0)= 1 :neigh(25,1)=-1 :neigh(25,2)= 1
Global last_neigh=5 ;(use 5, 17 Or 25 - 5 will avoind diagonal searches, 17 will avoid 3D-exclusive-diagonal searches)

; squareroot lookup table 
Global max_sqr=100000
Dim t_sqr(max_sqr)
For i=0 To max_sqr
 t_sqr(i)=Sqr(i)
Next

; initialize the map with random data
; 70% chance of plains
; 10% chance of brush/forest
; 10% chance of hills
; 10% chance of obstacle

; 0=Obstacle
; 1-3=Terrain
; 1=Easy
; 2=Med. 
; 3=Hard


; btw - I replaced the med and hard by easy to make the resulting path look more logical
For x = 0 To mapsize-1
	For y = 0 To mapsize-1
		For z = 0 To mapsize-1
	
			map.tile(x, y, z) = newTile()
			map(x, y, z)\x = x
			map(x, y, z)\y = y
			map(x, y, z)\z = z
		
			which = Rnd(1, 10)
			
			If (which <= 7)
				map(x, y, z)\walkable = True
				map(x, y, z)\g = 1 ; 1=easy to walk

			ElseIf which = 8
				map(x, y, z)\walkable = True
				map(x, y, z)\g = 1 ; also try 2=not so easy to walk

			ElseIf which = 9
				map(x, y, z)\walkable = True
				map(x, y, z)\g = 1 ; also try 3=pretty hard to walk
				
			Else
				map(x, y, z)\walkable = False 
				map(x, y, z)\g = 0 ; 0=impossible to walk
			EndIf
		
		Next
	Next
Next

; initialize start and goal locations randomly

While (map(start\x, start\y, start\z)\walkable = False) Or (randStart = False)
	randStart = True
	start\x = 0 ;Rand(0, mapsize-1)
	start\y = Rand(0, mapsize-1)
	start\z = Rand(0, mapsize-1)
Wend

While (map(goal\x, goal\y, goal\z)\walkable = False) Or (randGoal = False)
	randGoal = True
	goal\x = mapsize-1 ;Rand(0, mapsize-1)
	goal\y = Rand(0, mapsize-1)
	goal\z = Rand(0, mapsize-1)
Wend


; create a demo scene of cubes

For x = 0 To mapsize-1
	For y = 0 To mapsize-1
		For z = 0 To mapsize-1

			If map(x, y, z)\g=0 ; obstacle!
        	c=CreateCube() 
			PositionEntity c,gridsize#* x, gridsize#* y, gridsize#* z 
	 		EntityAlpha c,.5
			EndIf
			If Not ((x = start\x And y = start\y And z = start\z) Or (x = goal\x And y = goal\y And z = goal\z))

			ElseIf (x = start\X And y = start\y  And z = start\z)
	        	c=CreateCube() ; start point
				PositionEntity c,gridsize#* x , gridsize#* y, gridsize#* z 
				EntityAlpha c,.5 
				EntityColor c,0, 128, 255 
				rem_x#=gridsize#* x ; (used for helper pivot)
				rem_y#=gridsize#* y
				rem_z#=gridsize#* z
			Else
	        	c=CreateCube() ; end point
				PositionEntity c,gridsize#* x, gridsize#* y, gridsize#* z 
				EntityAlpha c,.5 
				EntityColor c,0, 255, 0 
			EndIf
		Next	
	Next
Next

; setup cam etc.
center=CreateCamera()
PositionEntity center,(mapsize/2)*gridsize#,(mapsize/2)*gridsize#,(mapsize/2)*gridsize#
camera=CreateCamera()
light=CreateLight()
helper=CreatePivot()
PositionEntity camera,17,40,-18
PointEntity camera,center
RenderWorld()

Color 255, 0, 0
Text 0, 0, "Press a key to plot path..."
Flip
WaitKey()


; actually find the path...
begin = MilliSecs()
go.waypoint = getWay(start, goal)
elapsed = MilliSecs() - begin








SetBuffer BackBuffer()
.main ;******************************************************************** Main loop

While go <> Null

	While myNext > MilliSecs(): Delay 10:Wend
	myNext = MilliSecs() + 300

	loc_x#=(gridsize#* go\x) ; waypoints in 3d
	loc_y#=(gridsize#* go\y)
	loc_z#=(gridsize#* go\z)
	
	c=CreateSphere() ; add some meshes to display the path...
	PositionEntity c,loc_x#, loc_y#, loc_z#  
	EntityColor c,255,0,0 
	EntityAlpha c,0.5 
	c=CreateCube() 
	FitMesh c,-.25,-.25,0,.5,.5,gridsize#*1.41
	PositionEntity c,loc_x#, loc_y#, loc_z#  
	EntityColor c,255,0,0 
	EntityAlpha c,0.5 
	PositionEntity helper,rem_x#,rem_y#,rem_z#
	PointEntity c,helper
	rem_x#=loc_x#
	rem_y#=loc_y#
	rem_z#=loc_z#

	RenderWorld() 
	Flip 

	go = go\myNext
	count = count + 1
Wend	

; and display the scene
While Not KeyDown(1)
;    cama#=(cama#+1)Mod 360
    cama#=MouseX()
	PositionEntity camera,((mapsize/2)*gridsize#)+Sin(cama#)*20,30,((mapsize/2)*gridsize#)+Cos(cama#)*20
	PointEntity camera,center
	RenderWorld() 
	Text 0, 0 , count + " waypoints generated"
	Text 0, 12, elapsed + " millisecs taken"
	Text 0, 24, "Move the mouse to rotate the scene"
	Flip 
Wend	



WaitKey
End










; plug in two coordinate sets, get back the first waypoint in the best path to the AI goal.
Function getWay.waypoint(start.coord, goal.coord)

	; prime the pump
	addList(open, map(start\x, start\y, start\z)\myNode)
	
; while the goal isn't on the open list, and while there are still open tiles to check.
	While (Not isList(open, map(goal\x, goal\y, goal\z)\myNode)) And (countList(open) > 0)	

		bestF.tile = getLowF(open, goal)
		xferList(bestF\myNode, closed)
		evalNeighbors(bestF)
		
	Wend
	
	RVAL.waypoint = genWay(start, goal)
	
	Return RVAL

End Function

; return the tile with the lowest F value
; F being a function of G + H, where
; G is the movement cost of a tile, and
; H is the guessed cost of movement to the goal.
Function getLowF.tile(myList.list, goal.coord)

	Local bestF% = 99999
	
	setPoint(myList, "START")
	
	For i = 1 To countList(myList)
		this.node = getPoint(myList)
		test.tile = this\parent
		; this still needed a diagonal G cost correction, sorry ppl
	 	myG = test\G + 10 				; a little tinkering here. -- this =should= be just plain G.
										; doing this makes avoiding bad terrain a slightly higher priority
		myH = getManhattan(test, goal)
		myF = myG + myH
		
		If myF <= bestF
			RVAL.tile = test
			bestF = myF
		EndIf
		setPoint(myList, "NEXT")
	Next
	
	Return RVAL

End Function


; simple distance heuristic.  difference in straight-lines multiplied by 10.
; might modify this to peek at terrain too.
Function getManhattan(this.tile, where.coord)

	mX = (Abs(this\x - where\x) * 1) 
	mY = (Abs(this\y - where\y) * 1) 
	mZ = (Abs(this\z - where\z) * 1) 
;	Return mx + my + mz 
    ; or maybe we better use a true pathagoras with square-root lookup table instead ?:
	loc_d=mx*mx + my*my + mz*mz
	If loc_d>max_sqr Then RuntimeError "Error: Need more precaculated square-roots in array t_sqr()!"
	Return t_sqr(loc_d)

End Function

; check neighbors for walkability and for possible pathing.
Function evalNeighbors(this.tile)

For i=0 To last_neigh ; to 5 or to 17 to do partial diagonal search Or to 25 to include full diagonal search
ckX=neigh(i,0)
ckY=neigh(i,1)
ckZ=neigh(i,2)
		
		If (this\x + ckX < 0) Or (this\x + ckX > (mapsize-1)) Or (this\y + ckY < 0) Or (this\y + ckY > (mapsize-1))  Or (this\z + ckZ < 0) Or (this\z + ckZ > (mapsize-1))
		
			; do nothing; this tile is out of range
			
		Else
		
			check.tile = getTile(this\x + ckX, this\Y + ckY, this\Z + ckZ)

			If (Not check\walkable) Or (isList(closed, check\myNode))
			
				; do nothing; this tile cannot be used
				
			Else
			
				If (Not isList(open, check\myNode))
					addList(open, check\myNode)
					addParent(check, this)
				Else
					If check\g <= this\parent\g
						this\parent = check
;						check\parent = this
					EndIf
				EndIf
			
			EndIf
			
		EndIf

Next		
End Function


Function getTile.tile(x, y, z)
	If x < 0 Or y < 0 Or z < 0 Or x > (mapsize-1) Or y > (mapsize-1) Or z > (mapsize-1)
		Return Null
	Else
		Return map(x, y, z)
	EndIf
End Function


Function genWay.waypoint(start.coord, goal.coord)

	x = goal\x
	y = goal\y
	z = goal\z
	
	myTile.tile = getTile(x, y, z)
	
	If myTile\parent = Null
		Return Null
	EndIf
	
	Repeat
	
		myWay.Waypoint = New Waypoint
		myWay\x = x
		myWay\y = y
		myWay\z = z
		RVAL.waypoint = addWay(myWay, RVAL)
		myTile = myTile\parent
		x = myTile\x
		y = myTile\y
		z = myTile\z
		
		If x = start\x
			If y = start\y
			If z = start\z
				Exit
			EndIf
			EndIf
		EndIf
		
	Forever
	
	Return RVAL
	
End Function


Function addWay.waypoint(this.waypoint, RVAL.waypoint)

	this\myNext = RVAL
	Return this

End Function


; parent a tile to another tile
Function addParent(kidTile.tile, popTile.tile)
	kidTile\parent = poptile
End Function
;~IDEal Editor Parameters:
;~C#Blitz3D