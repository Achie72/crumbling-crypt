pico-8 cartridge // http://www.pico-8.com
version 41
__lua__
-- Crumbling Crypt
-- by achiegamedev


-- Add McGuffin (later)
-- Fix Movement
-- Sniffers (?)

function add_enemy(_x, _y, _type)
	local enemy = {
		x = _x,
		y = _y,
		canStep = 0,
		canSeePlayer = false,
		type = _type,
		bop = rnd({0.2,0.3,0.4}),
		offx = 0,
		offy = 0
	}
	if (_type == 3) or (_type == 6) then
		enemy.canStep = 1
	end
	add(enemies, enemy)
end

function add_pickup(_x, _y, _type)
	local p = {
		x = _x,
		y = _y,
		type = _type
	}
	add(pickups, p)
end

function digger_showcase()
	-- random walker on an 8x8 grid on map
	-- set rooms to 1 if there should be something
	-- move randomly on if we are good
	local roomNumber = rnd({4,5,6,7,8})+flr(player.level/2)
	local xStart,yStart = flr(rnd(8)),flr(rnd(8))
	mset(xStart,yStart,1)
	-- init the dungeon with a starer point
	local diggerX, diggerY = xStart, yStart
	-- select a direction to dig to
	local dir = rnd({1,2,3,4})
	-- offset tables for the directions, left, right, up, down
	local dirTable = {{-1,0}, {1,0}, {0,-1}, {0, 1}}
	-- dig
	while (number_of_rooms() < roomNumber) do
		-- randomly turn into a new direction
		turn_chance = 0.25
		if rnd() < turn_chance then
  			dir = rnd({1,2,3,4})
		end
		-- calc new position that the digger would go to
		local newX, newY = diggerX+dirTable[dir][1],diggerY+dirTable[dir][2]
		-- check if it is inside the map
		while ((newX < 0) or (newX > 7) or (newY < 0) or (newY > 7)) do
			-- if not, find a new direction, check that
			dir = rnd({1,2,3,4})
			newX, newY = diggerX+dirTable[dir][1],diggerY+dirTable[dir][2]
		end
		-- set the new room
		mset(newX, newY, 1)
		-- set the new digger pos
		diggerX,diggerY = newX,newY
	end
end


function generate_dungeon()
	-- random walker on an 8x8 grid on map
	-- set rooms to 1 if there should be something
	-- move randomly on if we are good
	local roomNumber = 4+flr(player.level/2)
	local xStart,yStart = flr(rnd(8)),flr(rnd(8))
	mset(xStart,yStart,1)
	-- init the dungeon with a starer point
	local diggerX, diggerY = xStart, yStart
	-- select a direction to dig to
	local dir = rnd({1,2,3,4})
	-- offset tables for the directions, left, right, up, down
	local dirTable = {{-1,0}, {1,0}, {0,-1}, {0, 1}}
	-- dig
	while (number_of_rooms() < roomNumber) do
		-- randomly turn into a new direction
		turn_chance = 0.25
		if rnd() < turn_chance then
  			dir = rnd({1,2,3,4})
		end
		-- calc new position that the digger would go to
		local newX, newY = diggerX+dirTable[dir][1],diggerY+dirTable[dir][2]
		-- check if it is inside the map
		while ((newX < 0) or (newX > 7) or (newY < 0) or (newY > 7)) do
			-- if not, find a new direction, check that
			dir = rnd({1,2,3,4})
			newX, newY = diggerX+dirTable[dir][1],diggerY+dirTable[dir][2]
		end
		-- set the new room
		mset(newX, newY, 1)
		-- set the new digger pos
		diggerX,diggerY = newX,newY
	end
	local dungeonString =""
	-- gather digger data
	-- dungeon is the collection for the
	-- eventual dungeon data (room, not room)
	-- dungeon string is debug basically
	for x=0,7 do
	 	for y=0,7 do
	 		if (mget(x,y) == 1) then
				-- if the digger has a room there, add it to the dungeon
				-- +1-s so the array is indexed correctly(array from 1, map from 0)
				dungeon[x+1][y+1] = 1
				-- reset the map tile to empty once we added it
				mset(x,y, 0)

				dungeonString..="1"
			else
				-- if it was never set by the digger, no need to reset it
				dungeonString..="0"
			end
		end
		dungeonString..="\n"
	end

	-- put in the dummy filler for the dungeon
	for x = 0,127 do
		for y = 0,127 do
			mset(x,y, 63)
		end
	end


	-- here we buiild the actual map based on the 
	-- data inside dungeon. basically we blow up
	-- the 8x8 "dot" array, to a 8x8, 16x16 rooms (8x8 rooms filling the whole screen)
	for x=1,8 do
	 	for y=1,8 do
			-- if the dungeon has a room in positions
	 		if (dungeon[x][y] == 1) then
				-- generate the rooms on the map
				-- this is done by grabbing the corners and
				-- multiplying them by 16 so we start at the right
				-- map coordinates
				generate_room((x-1)*16, (y-1)*16)
				-- try to fix room entrances
				for i=1,4 do
					-- we grab the neighbouring tiles from the dungeon based
					-- on the direction data
					local newCellX, newCellY = x+dirTable[i][1],y+dirTable[i][2]
					-- if this new cell is inside the "dungeon"
					if not ((newCellX > 8) or (newCellX < 1) or (newCellY > 8) or (newCellY < 1)) then
						-- check if it is a room, if yes, create the
						-- "door" between the current room and the new one
						if dungeon[newCellX][newCellY] == 1 then
							-- locally doors should be on
							-- left
							-- {0,7},{0,8}
							-- right
							-- {15, 7}, {15, 8}
							-- up
							-- {7,0},{8,0}
							-- down
							-- {7, 15}, {8,15}
							-- offset to map coords (0 indexed vs 1 indexed array)
							local xMap, yMap = x-1,y-1
							-- set the wall tiles to doors based on direction
							if i==1 then
								-- mset the tile on the map, with the positions
								-- and the offsets above.
								mset( xMap*16, yMap*16+7, 61)
								mset( xMap*16, yMap*16+8, 61)
							elseif i==2 then
								mset( xMap*16+15, yMap*16+7, 61)
								mset( xMap*16+15, yMap*16+8, 61)
							elseif i==3 then
								mset( xMap*16+7, yMap*16, 61)
								mset( xMap*16+8, yMap*16, 61)
							elseif i==4 then
								mset( xMap*16+7, yMap*16+15, 61)
								mset( xMap*16+8, yMap*16+15, 61)
							end
						end
					end
				end
			end
		end
	end

	-- add the brick part to the top wall
	-- if the bottom is empty
	local emptySpaceCollection = {}
	-- loops throught the whole map
	for x = 0,127 do
		for y = 0,127 do
			-- if it is an empty tile add it to the emptySpace
			-- collection to spawn stuff later oon
			if (mget(x,y) != 1) and (mget(x,y) != 63) then
				local point = {x=x,y=y}
				add(emptySpaceCollection,point)
			end
			-- if it is a wall on top, that means it is a 1 tile
			-- wall, set it to the smaller wall tile
			if (mget(x,y-1) == 1) and (not (mget(x,y)==1)) then
				mset(x,y-1,2)
			end
		end
	end

	

	-- find a spot for the player
	local playerStarterPos = rnd(emptySpaceCollection)
	del(emptySpaceCollection, playerStarterPos)

	-- if we are on the 10th level spawn the mcguffin
	if (player.level == 10) then
		local mcguffinPos = rnd(emptySpaceCollection)
		add_pickup(mcguffinPos.x, mcguffinPos.y, 5)

		-- add guardian
		local guardianSpot = rnd(emptySpaceCollection)
		del(emptySpaceCollection, guardianSpot)
		add_enemy(guardianSpot.x, guardianSpot.y, 7)
	else
		-- create random trapdoor down
		local downPos = rnd(emptySpaceCollection)
		mset(downPos.x, downPos.y, 55)
		-- save the position so we can check against
		-- runtime to see if we are on the door
		doorDown.x = downPos.x
		doorDown.y = downPos.y
		del(emptySpaceCollection,downPos)

		-- find a spot for the key
		local keyPos = rnd(emptySpaceCollection)
		add_pickup(keyPos.x, keyPos.y, 4)
		del(emptySpaceCollection, keyPos)
	end


	-- add decals
	for space in all(emptySpaceCollection) do
		if rnd() > 0.92 then
			mset(space.x, space.y, rnd({0,0,0,0,0,56,57,58,59,60}))
		end
	end

	-- set the player's starting position
	-- and set the tile under to the stairs
	player.x = playerStarterPos.x
	player.y = playerStarterPos.y
	mset(playerStarterPos.x, playerStarterPos.y, 54)

end

-- room counter for the digger
function number_of_rooms()
	-- dig until we have set number of rooms
	-- so for that we just check which tiles are set
	local rooms = 0
	for x=0,7 do
	 	for y=0,7 do
	 		if mget(x,y) == 1 then
				rooms+=1
			end
		end
	end
	return rooms
end

function generate_room(x, y)
	-- for rooms we want to generate from a pre-build corner set
	-- we choose a random preset for each corner
	local corners = {0,0,0,0}
	for i=1,4 do
		--printh("selecting corners")
		local rand = flr(rnd(4)) + ((i-1)*4)
		corners[i] = rand

		--printh(rand.." for corner: "..i)
	end

	-- then we pass the corner "collection" to the builder
	-- to actually build the tiles
	for i=0,3 do
		build_room(corners[i+1], i, x, y)
	end
end

function build_room(corner_sprite_id, corner, _x, _y)
	--printh("building room at: ".._x.." ".._y)
	local idString = ""
    -- indicate where the given sprite starts on
    -- the sprite sheet.
    local xStart = 0
    local yStart = 0
    xStart = corner_sprite_id*8
	yStart = 32
    --printh("corner "..corner.." sprite "..spr)


    -- gather pixel color values
    --printh("gathering colors")
	-- we start with y so we gather the x coords first
	-- so our mset builder down below builds correctly
    for y=0,7 do
        for x=0,7 do
            -- gather the colors from the sprite.
            -- it starts from the sprite number*8, and all of them are at y=32 on the sprite sheet
            -- but this could be configurated later for more
            idString = idString..sget(xStart+x, yStart+y)..","
        end
    end
    -- where to start the build based on which corner we
    -- are doing at the moment. Top left corner is 0,0, top right is 8,8,
    -- bottom right is 8,8, bottom left is 0,8
    local mapCornerTable = {{0,0}, {8,0}, {8,8}, {0,8}}
    local mapBuildStartX = mapCornerTable[corner+1][1]+_x
    local mapBuildStartY = mapCornerTable[corner+1][2]+_y
    local x,y = 0,0
    -- now we just loop over the colors and place the
    -- tiles corresponding to the color.
    for val in all(split(idString)) do
        --printh("setting: "..x..","..y.."to: "..val)
        if val == 1 then
        --dark blue - wall
            mset(x+mapBuildStartX,y+mapBuildStartY, 1)
			--printh("set at: "..x+mapBuildStartX.." "..y+mapBuildStartY)
        elseif val == 2 then
        -- dark red, enemy
			if rnd() > 0.3-(player.level/10) then
            	--mset(x+mapBuildStartX,y+mapBuildStartY, 5)
				local enemyCollectionToSpawnFrom = {1,1,1}
				-- add harder enemies into the spawning chance based on
				-- how deep we are into the crypt
				if player.level >= 3 then add(enemyCollectionToSpawnFrom, 2) end
				if player.level >= 5 then add(enemyCollectionToSpawnFrom, 3) end
				if player.level >= 7 then add(enemyCollectionToSpawnFrom, 4) end
				if player.level >= 8 then add(enemyCollectionToSpawnFrom, 5) end
				add_enemy(x+mapBuildStartX,y+mapBuildStartY, rnd(enemyCollectionToSpawnFrom))
			end
			mset(x+mapBuildStartX,y+mapBuildStartY, 0)
        elseif val == 3 then
        -- dark green, pickup
			if rnd() > 0.5 then
            	add_pickup(x+mapBuildStartX,y+mapBuildStartY, rnd({1,2}))
			end
			-- set the tile to 0 so it can be looked in
			-- while running the A*
			mset(x+mapBuildStartX,y+mapBuildStartY, 0)
        elseif val == 0 then
			-- set the tile to 0 so it can be looked in
			-- while running the A*
			mset(x+mapBuildStartX,y+mapBuildStartY, 0)
		end
        x+=1
        -- this is to convert the 1D array
        -- into 2D world coordinates. At every 8th tile we go
        -- to the next row.
		if x > 7 then
			x = 0
			y += 1
		end
    end

end


-- calc manthattan distance, which is the grid based
-- tile distance between two points
function heuristic(a, b)
	return (abs(a.x - b.x)+ abs(a.y-b.y))
end

-- Insertion Sort by @impbox
function sort(array)
    for i=1,#array do
        local j = i
        while j > 1 and array[j-1].f > array[j].f do
            array[j],array[j-1] = array[j-1],array[j]
            j = j - 1
        end
    end
end

-- scuffed sorter for an array with "holes"
function sort_holey(array)
	local nonHoledArray = {}
	for _,node in pairs(array) do
		if (node != nil) and ( node != {}) then

			add(nonHoledArray, node)
			::skip::
		end	
	end
	sort(nonHoledArray)
	return nonHoledArray
end

-- pop from sorted array for array with holes
-- gather non-hole elements, sort them, pop
function pop_sort(array)
	local nonHoledArray = {}
	for _,node in pairs(array) do
		if (node != nil) and ( node != {}) then

			add(nonHoledArray, node)
			::skip::
		end	
	end
	sort(nonHoledArray)
	local node = nonHoledArray[1]
	array[node.x+node.y*128] = nil
	return node
end

-- starting to get jank, but is a check for spaces
-- to see if an enemy can step/spawn there, basically
-- empty tile check
function is_legal_spot(spot, enemy, enemyPosTable)
	local x = spot.x
	local y = spot.y

	-- is the spot already taken
	if (enemyPosTable[x+y*128] == "taken") then return false end
	-- is it inside the map
	if (x < 0) or (x > 128) or (y < 0) or (y > 128) then
		return false
	end
	return true
end

-- helper to check against all tiles that are walls
function wall_tile_helper(spot,canBreakWalls)
	if canBreakWalls then return true end
	local x = spot.x
	local y = spot.y
	--land = {1}
	--0
	local tileId = mget(x,y)
	-- walls that are breakable are 1,2 and 64
	if (tileId == 1) or (tileId == 2) or (tileId == 63) then
		return false
	end
	return true
end

-- find existing neighbour tiles that are walkable (non walls)
function get_neighbours(pos, enemy, enemyPosTable)
	local neighbours={}
	local xPos = pos.x
	local yPos = pos.y

	-- classic offsets to index tiles from the center
	-- tile we are calling this with
	local up = {x = xPos, y=yPos-1}
	local down = {x = xPos, y=yPos+1}
	local left = {x = xPos-1, y=yPos}
	local right = {x = xPos+1, y=yPos}


	if is_legal_spot(up, enemy, enemyPosTable) and wall_tile_helper(up,enemy.type==4) then
		add(neighbours,up)
	end

	if is_legal_spot(down, enemy, enemyPosTable) and wall_tile_helper(down,enemy.type==4) then
		add(neighbours,down)
	end

	if is_legal_spot(left, enemy, enemyPosTable) and wall_tile_helper(left,enemy.type==4) then
		add(neighbours,left)
	end

	if is_legal_spot(right, enemy, enemyPosTable) and wall_tile_helper(right,enemy.type==4) then
		add(neighbours,right)
	end

	return neighbours
end

-- check if the array already has an element
-- at the indexed point
function is_node_inside_set(set, index)
	if set[index] == nil then return false end
	if set[index] == {} then return false end
	return true
end

--- a* from colony management,
-- janked up, because sniffers are
-- my new nightmare. For more in depth
-- explanation take a look at:
-- https://www.redblobgames.com/pathfinding/a-star/introduction.html
-- enemyPositions is my jank solution to
-- not let them step on others, and help skip
-- a few checks. Also handy for slimes
function find_path(enemy, goal)

	openSet = {}
	openIndexedSet = {}
	closedIndexedSet = {}
	closedList = {}
	startNode = {x=enemy.x, y=enemy.y}
	startNode.g = 0
	startNode.h = heuristic(startNode, goal)
	startNode.f = startNode.g + startNode.h
	startNode.parent = 0
	endNode = goal
	endNode.g = 0
	endNode.h = 0
	endNode.f = 0
	enemy.path = {}

	add(openSet, startNode)
	

	local enemyPositions = {}

	for otherEnemy in all(enemies) do
		if enemy != otherEnemy then
			if on_same_screen(enemy, otherEnemy) then
				enemyPositions[otherEnemy.x+otherEnemy.y*128] = "taken"
			end
		end
	end

	openIndexedSet[enemy.x+enemy.y*128] = startNode
	local openSetCount = 1
	local iteration = 0

	while (openSetCount > 0 and openSetCount < 1000) do
		-- i should sort here
		
		-- pop the least value out of the open
		local current = pop_sort(openIndexedSet)


		-- if we are on the goal, trace back on the parents and add those to the
		-- path
		if (current.x == goal.x) and (current.y == goal.y) then
			while not (current == startNode) do
				add(enemy.path, current)
				current = current.parent
			end
			enemy.closedList = {}
			for _,p in pairs(closedIndexedSet) do
				if not (p==nil) then
					add(enemy.closedList,p)
				end
			end
			return
		end

		-- add node to closed
		-- add(closedList, current)
		closedIndexedSet[current.x+current.y*128] = current


		-- get valid neighbours
		local neighbours = get_neighbours(current, enemy, enemyPositions)
		for node in all(neighbours) do
			

			-- check if item is alredy in the closed
			local alreadyInClosed = false
			if is_node_inside_set(closedIndexedSet, node.x+node.y*128) then alreadyInClosed = true end

			-- if not in closed
			if not alreadyInClosed then

				node.g = current.g
				node.h = heuristic(goal, node)
				node.f = node.g+node.h

				-- check if node is already in the openList
				local alreadyInOpen = false

				if is_node_inside_set(openIndexedSet, node.x+node.y*128) then
					alreadyInOpen = true
					if openIndexedSet[node.x+node.y*128].g >= node.g then
						openIndexedSet[node.x+node.y*128].parent = current
						openIndexedSet[node.x+node.y*128].g = node.g
						openIndexedSet[node.x+node.y*128].f = node.f
						openIndexedSet[node.x+node.y*128].h = node.h
						--openIndexedSet = sort_holey(openIndexedSet)
					end
				end

				-- if the node is not in the open, the add node to the open
				if (not alreadyInOpen) then
					node.parent = current
					--add(openSet,node)
					openIndexedSet[node.x+node.y*128] = node
				end
			end	

		end

		-- count elements
		local count = 0
		for _,node in pairs(openIndexedSet) do
			if (node != nil) and (node != {}) then
				count += 1
			end	
		end
		openSetCount = count
		iteration += 1
	end

	enemy.closedList = {}
	for _,p in pairs(closedIndexedSet) do
		if not (p==nil) then
			add(enemy.closedList,p)
		end
	end


end

-- line of sight with bresenham lines
-- not sure from where, thanks for whoever you are!
-- basically draw a line between two tiles, and see
-- if any tiles that are along the line are wall tiles
-- if yes, we cannot see, if no, we can see spot 1 (x0,y0) from
-- spot2 (x1,y1)
-- more info on: https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
function line_of_sight(x0,y0,x1,y1)

  local sx,sy,dx,dy

  if x0 < x1 then
    sx = 1
    dx = x1 - x0
  else
    sx = -1
    dx = x0 - x1
  end

  if y0 < y1 then
    sy = 1
    dy = y1 - y0
  else
    sy = -1
    dy = y0 - y1
  end

  local err, e2 = dx-dy, nil


  while not(x0 == x1 and y0 == y1) do
    e2 = err + err
    if e2 > -dy then
      err = err - dy
      x0  = x0 + sx
    end
    if e2 < dx then
      err = err + dx
      y0  = y0 + sy
    end
	local tile = mget(x0, y0)
    if (tile == 1) or (tile == 2) then return false end
  end

  return true
end

-- check if to entity-s are on the same screen
function on_same_screen(entityA, entityB)
	return (flr(entityA.x/16) == flr(entityB.x/16)) and (flr(entityA.y/16) == flr(entityB.y/16))
end

-- tooltip startup
-- my tooltip as an x position
-- that is where currently it is,
-- a stay timer, which decides how long
-- it is on screen
function init_tooltip(text, stay)
    tooltip = {
        text=text,
        startX = -100,
        x = -100,
        stay = stay,
        timer = stay+20
    }
end

-- from pancelor's gamedev blog about input
-- buffering in grid games
function approach(x, target, speed)
	if speed == nil then speed = 1 end
	return x < target and min(x + speed,target) or max(x - speed, target)
end

-- huge init
function _init()
	music(10)
	player = {
		x = 2,
		y = 2,
		sword = true,
		shield = false,
		potion = 0,
		steps = 2,
		level = 1,
		screenX = 0,
		screenY = 0,
		offx=0,
		offy=0
	}

	dungeon = {
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 }
	}

	enemies = {}
	pickups = {}
	state = "menu"

	tooltip = {
		text="",
		startX = -100,
		x = -100,
		stay = 0,
		timer = 0
	}

	--init_tooltip("floor 1", 20)

	-- color keep
	poke(0x5f2e,1)
	-- poke bigger map
	--reload(0x8000, 0x8000, 0x7fff)
	memset(0x8000, 0, 0x4000)
	poke(0x5f56, 0x80)
	poke(0x5f57, 128)
	-- palette
	pal({[0]=129,13,2,3,4,5,6,7,8,9,10,11,12,13,14,15},1)
	doorDown = {}
	generate_dungeon()
	--digger_showcase()
	--player.x, player.y = 65,65
	transition = 0
	player.screenX = flr(player.x/16)*128
	player.screenY = flr(player.y/16)*128
	musicToggle = true
	menuitem(1, "music "..tostr(musicToggle), toggle_music)
end

-- this is the function that is called
-- by the menuitem toggle music
-- basically play or stop music
function toggle_music()
    musicToggle = not musicToggle
    if musicToggle then
        music(10)
    else
        music(-1,300)
    end
end

-- create a new level to be played
function load_level()
	-- increase level we are on
	player.level += 1
	-- we don't have the key, so we
	-- cannot enter the trapdoor down
	-- unti we find it again
	player.key = false
	-- reset the digger's data, basically
	-- it digs on an 8x8 area an that is blow
	-- up into 16x16 rooms
	dungeon = {
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 },
		{0, 0, 0, 0, 0, 0, 0, 0 }
	}

	-- reset enemies, pickups and the state
	enemies = {}
	pickups = {}
	state = "player"

	-- reset tooltip
	tooltip = {
		text="",
		startX = -100,
		x = -100,
		stay = 0,
		timer = 0
	}

	-- creates the "floor x" float-in text
	init_tooltip("floor "..player.level, 20)

	-- poke bigger map
	--reload(0x8000, 0x8000, 0x7fff)
	memset(0x8000, 0, 0x4000)
	
	-- forget the old door position
	doorDown = {}
	-- generate the dungen
	generate_dungeon()
end

function _update()

	if state == "menu" then
		update_menu()
	elseif state == "player" then
		-- player's turn
		update_player()
	elseif state == "enemies" then
		-- enemy turn
		update_enemies()
	elseif state == "dead" then
		update_dead()
	elseif state == "won" then
		update_won()
	end

	-- set the camera's position
	local camPosX = player.screenX
	local camPosY = player.screenY

	camera(camPosX, camPosY)

	-- tooltip lerping, this could be heavly
	-- optimized by using a lerp library, for ex
	-- from FletchMakes
	if tooltip.timer > 0 then
        local midpoint = player.screenX+63-(print(tooltip.text, 0, -200, 7))/2
        if tooltip.x < midpoint then
            tooltip.x += (midpoint - tooltip.x)/4
            if abs(tooltip.x-midpoint) < 0.5 then
                tooltip.stay -= 1
            end
        end
        if tooltip.stay <= 0 then
            tooltip.x += (player.screenX+200 - tooltip.x)/4
            if tooltip.x > player.screenX+180 then
                tooltip.timer = 0
                tooltip.x = player.screenX-100
            end
        end
    end

	-- reduce the tooltip's timer so it actually goes
	-- offscreen
	if tooltip.timer == 0 then
		tooltip.x = player.screenX-100
	end

end

function update_menu()
	-- start the game
	if btnp(5) then
		state = "player"
		--_init()
	end
end

function update_won()
	-- return to the menu
	-- and regen a first level
	-- so we have a nice backdrop in the menu
	if btnp(5) then
		state = "menu"
		player.level = 0
		player.sword = false
		player.shield = 0
		load_level()
	end
end

function update_player()

	-- player lerping between two positions
	if not (player.offx==0 and player.offy==0) then
		player.offx = approach(player.offx, 0, 3)
		player.offy = approach(player.offy, 0, 3)
	else
		-- direction table, that will tell us
		-- which button press moves the player
		-- in which direction as in x,y pairs.
		-- so button 0 - left is moving us -1 on x and
		-- 0 on the y. Comes in handy in a few checks
		local dirTable = {{-1,0}, {1,0}, {0,-1}, {0, 1}}
		-- actualy button direction we pressd
		local dir = 0

		-- save player position into new_x so we can
		-- modifiy position runtime, do checks, and if we cannot
		-- move we just don't assign new_x, new_y back to the player
		local new_x, new_y = player.x, player.y
		local moved = false
		-- moving and saving direction
		if btnp(0) then
			new_x = player.x - 1
			moved = true
			dir = 1
		elseif btnp(1) then
			new_x = player.x + 1
			moved = true
			dir = 2
		elseif btnp(2) then
			new_y = player.y - 1
			moved = true
			dir = 3
		elseif btnp(3) then
			new_y = player.y + 1
			moved = true
			dir = 4
		end
		-- if we moved
		if moved then
			player.oldX, player.oldY = player.x, player.y
			--check if can step there
			-- flag 0 is walls
			if not (fget(mget(new_x, new_y), 0)) then
				player.x = new_x
				player.y = new_y
			end
			-- remove one of our 2 steps
			player.steps -=1
			sfx(0,3)

			-- check if enemy is in the way
			for i=#enemies,1,-1 do
				local enemy = enemies[i]
				-- only check things if the enemy is
				-- on the same screen as us
				if on_same_screen(enemy, player) then
					local fought,dead = check_for_combat(enemy, player)
					-- if we fought then do not step
					if fought and (not dead) then
						player.x = player.oldX
						player.y = player.oldY
					end
				end
			end
			player.offx = dirTable[dir][1]*-8
			player.offy = dirTable[dir][2]*-8
		end


		-- handle all the pickup collisions
		-- we init the tooltip to say what
		-- we picked up, set the player according to it
		-- play a sound, and reset the tile under it, so it can be
		-- checked in A*
		for pickup in all(pickups) do
			if on_same_screen(pickup, player) then
				if (pickup.x == player.x) and (pickup.y == player.y) then
					if (pickup.type == 1) and (not player.shield) then
						player.shield = true
						init_tooltip("shield acquired", 20)
						sfx(1,3)
						del(pickups, pickup)
						mset(pickup.x*8, pickup.y*8, 0)
					elseif (pickup.type == 2) and (not player.sword) then
						player.sword = true
						init_tooltip("sword acquired", 20)
						sfx(1,3)
						del(pickups, pickup)
						mset(pickup.x*8, pickup.y*8, 0)
					elseif (pickup.type == 4) then
						player.key = true
						init_tooltip("◆-- key found --◆", 20)
						sfx(5,3)
						del(pickups, pickup)
						mset(pickup.x*8, pickup.y*8, 0)
					elseif (pickup.type == 5) then
						-- this is the mcguffin
						state = "won"
					end
				end
			end
		end

		-- did we find the door, and do we have the key
		if moved and (player.x == doorDown.x) and (player.y == doorDown.y) then
			if (not player.key) then
				-- if no say it is locked
				init_tooltip("!-- locked --!", 20)
				sfx(4,3)
			else
				-- if yes, load new level
				load_level()
			end
		end

		-- calculate new player's screen position
		player.screenX = flr(player.x/16)*128
		player.screenY = flr(player.y/16)*128

		-- if we ran out of our steps, and we are not dead, let
		-- the enemies step
		if (player.steps == 0) and (not (state == "dead")) then
			state = "enemies"
		end
	end
end

-- this handles all the enemy logic in the game
function update_enemies()
--printh("=======")
	for i=#enemies,1,-1 do
		local enemy = enemies[i]
		-- grab the enemy to act
		local sameScreen =  on_same_screen(enemy, player)
		-- enemies only act if they are on the same screen as the player
		-- expcet the sniffers as the wander into your room if they can
		-- see you from their position in neighbouring rooms
		if sameScreen or (enemy.type == 2)then
			-- does the enemy have the ability to step?
			-- this number is higher if the enemy is stunned by the shield
			if (enemy.canStep == 0) then
				-- brutes do not care at all of line of sight, they just go to the 
				-- player if on same screen
				-- otherwise only move the enemy if it can see and is close enough
				if (enemy.type == 4) or ((abs(enemy.x-player.x) < 16) and ((abs(enemy.y-player.y) < 16))) and line_of_sight(enemy.x,enemy.y,player.x,player.y) then
					enemy.canSeePlayer = true
					--("enemy pos:"..enemy.x.." "..enemy.y)
					-- call the a*
					find_path(enemy, player)
					local startX,startY = enemy.x, enemy.y
					local stepsToTake = 1
					-- steps the enemy does i it's turn. Enemy 3,
					-- the diggers step 2 times every two turns
					if enemy.type == 3 then stepsToTake = 2 end

					-- do the steps
					for i=1,stepsToTake do
						-- find the first step in the path towards player
						if #enemy.path > 0 then
							local popPath = enemy.path[#enemy.path]
							del(enemy.path, enemy.path[#enemy.path])
							
							-- move there
							enemy.x = popPath.x
							enemy.y = popPath.y
							
							-- brutes breaking walls
							if (mget(enemy.x,enemy.y) == 2) or (mget(enemy.x, enemy.y) == 1) then
								mset(enemy.x, enemy.y, 53)
							end
							
							-- enemies can also engage in combat if the step into us
							local fought,dead = check_for_combat(enemy, player)
							
							-- if we fought, then they cannot step into the new position
							if fought and (not dead) then
								enemy.x = startX
								enemy.y = startY
							end
							if dead then 
								break 
							end
						end
					end
					-- the little dig guy can step again
					if enemy.type == 3 then 
						enemy.canStep = 1
					end
				-- wandering code
				else
					enemy.canSeePlayer = false
					-- if not seeing player, randomly movve around
					local stepsToTake = 1
					if enemy.type == 3 then stepsToTake = 2 end
					-- randomly choose a direction to wander into
					if rnd() > 0.5 then
						for i=1,stepsToTake do
							-- try a few steps if we can actually step there
							local newX, newY, tries = 0, 0, 0 
							local isLegal, isWall = true, true
							repeat
								local dir = rnd({1,2,3,4})
								-- offset tables for the directions, left, right, up, down
								local dirTable = {{-1,0}, {1,0}, {0,-1}, {0, 1}}
								-- generate the possible next position to wander onto
								local spot = {
									x = enemy.x + dirTable[dir][1],
									y = enemy.y + dirTable[dir][2]
								}
								-- gather other enemies, and set their position onto
								-- non steppable
								local enemyPositions = {}
									for otherEnemy in all(enemies) do
										if enemy != otherEnemy then
											if on_same_screen(enemy, otherEnemy) then
												enemyPositions[otherEnemy.x+otherEnemy.y*128] = "taken"
											end
										end
									end
								-- check new pos if it is steppable
								isLegal = is_legal_spot(spot, enemy, enemyPositions)
								local tile = mget(spot.x, spot.y)
								isWall = (tile == 1) or (tile == 2)
								newX = spot.x
								newY = spot.y
								-- increase tries, if we are over 500 then skip wandering
								-- this can happen if the enemy is surrounded by other enemies
								tries += 1
							until (is_legal_spot) and (not isWall) or (tries > 500) 
							-- if we found a new step earlier then exiting because of
							-- no possible new steps, step onto that tile
							if not (tries > 500) then
								enemy.x = newX
								enemy.y = newY
							end 
						end -- for cycle for stepsToTake
					end -- if rnd() > 0.5
				end -- wander or chase if can step
			else
			-- if cannot step
				if enemy.canStep > 0 then
					enemy.canStep -=1
				end
			end
		end
	end
	-- after all enemies stepped, and the player is not
	-- dead then it is the players turn, set the step
	-- ammount to two, as the player can step twiche in a 
	-- turn
	if state != "dead" then
		state = "player"
		player.steps = 2
	end
end

-- really janky combat check
function check_for_combat(enemy, player, enemyPositions)
	-- are we on the same position as the enemy
	if (enemy.x == player.x and enemy.y == player.y) and (enemy.canStep == 0) then
		if player.sword then
			-- if the player has the sword, then the
			-- enemy is dead upon stepping into the same tile
			-- if we are fighting a slime
			if enemy.type == 5 then
				-- let's try to spawn enmeis if it is dead
				local spawned = 0
				local tries = 0
				-- gather enemies in the room
				local enemyPositions = {}
				-- gather possible spawning spots
				for otherEnemy in all(enemies) do
					if enemy != otherEnemy then
						if on_same_screen(enemy, otherEnemy) then
							-- mark spot as taken
							enemyPositions[otherEnemy.x+otherEnemy.y*128] = "taken"
						end
					end
				end
				-- try to spawn in little slimes
				while (spawned < 2)  do
					-- choose random direction
					local dir = rnd({1,2,3,4})
					-- offset tables for the directions, left, right, up, down
					local dirTable = {{-1,0}, {1,0}, {0,-1}, {0, 1}}
					local spot = {
						x = enemy.x+dirTable[dir][1],
						y = enemy.y+dirTable[dir][2]
					}
					-- check for legal spot and player not being there
					printh(mget(spot.x, spot.y))
					local tile = mget(spot.x, spot.y)
					if ((tile == 0) or (tile > 2)) and (not ((player.oldX == spot.x) and (player.oldY == spot.y)) and is_legal_spot(spot, enemy, enemyPositions)) then
						-- add in new slime
						add_enemy(spot.x, spot.y, 6)
						-- add slime position to taken, because we have more to spawn
						enemyPositions[spot.x+spot.y*128] = "taken"
						spawned +=1
					end
					-- try to spawn slimes, but sometimes we can't
					-- as there is not enough space, the if
					-- here helps us exit an endless check loop
					-- if there is no space to spawn slimes into
					tries += 1
					if tries > 100 then
						spawned = 2
					end
				end
			end


			-- for enemies other than slime
			-- delete the enemy
			del(enemies, enemy)
			-- "break" the sword
			player.sword = false
			-- init the tooltip
			init_tooltip("!-- sword broken --!", 20)
			-- play broken sound
			sfx(3,3)
			return true, true
			-- we fought and the enemy is dead
		elseif player.shield then
			-- if the player has the shield, then
			-- the enemy is stunned
			enemy.canStep = 2
			-- remove the player's shield
			player.shield = false
			init_tooltip("!-- shield broken --!", 20)
			sfx(2,3)
			return true, false
			-- we fought but the enemy is not dead
		else
			-- if the player doesn' have a shield
			-- or sword, then the enemy will kill us
			state = "dead"
		end
	end
	return false, false
	-- we haven't fought
end

function update_dead()
	-- restart the game
	if btnp(5) then
		_init()
	end
end

function _draw()
	cls()
	-- draw only the map part that is under the player???
	--map(flr(player.x/16), flr(player.y/16), 0, 0, 16, 16)
	-- what the actual fuck is going on :(
	-- draw everything from the get go????
	--map(0,0,0,0,128,128)

	-- default???

	for i=1,16 do
		rectfill(i*4,0,i*4+4,64,i-1)
	end

	map()

	for enemy in all(enemies) do
		-- background for enemies so you don't see the tile under
		rectfill(enemy.x*8, enemy.y*8, enemy.x*8+7, enemy.y*8+7, 0)

		-- enemy headbopping, base sprite is enemy.type+17,
		-- headbop sprite is +1
		if (sin(time())) <= enemy.bop then
			spr(17+(enemy.type-1)*2, enemy.x*8, enemy.y*8)
		else
			spr(16+(enemy.type-1)*2, enemy.x*8, enemy.y*8)
		end
		
		-- if the enemy is stunned then draw the little stun 
		-- particles on top of them
		if enemy.canStep > 0 then
			print_outline(rnd({"*","✽","◆","★","⧗"}), enemy.x*8+2, enemy.y*8-6, 1, 0)
		elseif enemy.canSeePlayer then
			-- if the enemy can see us, draw the eye above their head
			print_outline("☉", enemy.x*8, enemy.y*8-6, 1, 0)
		end

		-- offscreen sniffer indication
		-- after movement. check if the sniffer is on an edge tile
		-- if yes, show the eye on the edge of the screen so the player
		-- knows there is a sniffer in the doorstep
		if (enemy.type == 2) and (not (on_same_screen(enemy, player))) and (line_of_sight(enemy.x, enemy.y, player.x, player.y)) then
				printh("=====")
			local enemyScreenPosX = flr(enemy.x/16)
			local enemyScreenPosY = flr(enemy.y/16)
			local enemyNormalizedX = enemy.x - (enemyScreenPosX*16)
			local enemyNormalizedY = enemy.y - (enemyScreenPosY*16)
			local enemyXPos = enemyNormalizedX % 15
			local enemyYPos = enemyNormalizedY % 15
			-- if enemy is on "local" 0 or 15 tile
			-- then it is on the doorstep on the other neighbouring room

			local xOffset,yOffset = 0,0


			if (enemyXPos == 0) then
				-- do some kind of indication
				xOffset = (enemy.x < player.x) and 8 or -8
			elseif (enemyYPos == 0) then
				yOffset = (enemy.x < player.x) and 12 or -8
			end
			print("\#0".."😐", enemy.x*8+xOffset, enemy.y*8+yOffset, 1)
		else
			enemy.onDoorstep = false
		end
	end

	-- draw the pickupsTheir sprites start on 39 and
	-- adding the type to it will give their sprite in order
	for pickup in all(pickups) do
		if on_same_screen(pickup, player) then
			local offset = sin(time()%5)
			spr(39+pickup.type, pickup.x*8, pickup.y*8+offset)
		end
	end

	-- rectfill under the player, so tiles under doesn't mix into the sprite
	rectfill(player.x*8, player.y*8, player.x*8+7, player.y*8+7, 0)

	-- reall janky way to select he appropriate player sprite 
	-- based on which pickup we have. draw the player on it's
	-- position. If the player is lerping between two tiles,
	-- we add the offy,offx as well (this is the smooth movement
	-- between tiles) 
	if (not player.sword) and (not player.shield) then
		spr(4, player.x*8+player.offx, player.y*8+player.offy)
	elseif (player.sword) and (not player.shield) then
		spr(49, player.x*8+player.offx, player.y*8+player.offy)
	elseif (not player.sword) and (player.shield) then
		spr(48, player.x*8+player.offx, player.y*8+player.offy)
	else
		spr(50, player.x*8+player.offx, player.y*8+player.offy)
	end

	-- if the player has the key, draw it over the head
	if player.key == true then
		spr(43, player.x*8, player.y*8-8)
	end

	-- tooltip text
	print("\#0"..tooltip.text, tooltip.x, flr(player.y/16)*128+52, 1)

	-- if we are not in the menu, draw the floor's number on the bottom right
	if state != "menu" then
		print("\#1".."floor:"..player.level, flr(player.x/16)*128+96, flr(player.y/16)*128+122, 0)
	end
	-- we don't have a different draw call for our states, because i wanted
	-- the texts to appear over the actual gameplay screen you are on!
	-- if we are in the menu, draw the menu text over the first generated crypt!
	if state == "menu" then
		local title = "CRUMBLING\n  CRYPT"
		print("\#0\^w\^t"..title, flr(player.x/16)*128+64-#"crumbling"*4, flr(player.y/16)*128+10, 1)

		
		local subtitle = "a minimalist roguelike"
		print("\#0"..subtitle, flr(player.x/16)*128+64-#subtitle*2, flr(player.y/16)*128+40, 1)

		local start = "❎ to start"
		print("\#0"..start, flr(player.x/16)*128+64-#start*2, flr(player.y/16)*128+50, 1)	
	end
	-- dead text
	if state == "dead" then
		local endText = "perished on floor: "..player.level
		print("\#0"..endText, flr(player.x/16)*128+64-#endText*2, flr(player.y/16)*128+40, 1)

		local start = "❎ to start"
		print("\#0"..start, flr(player.x/16)*128+64-#start*2, flr(player.y/16)*128+50, 1)	
	end
	-- won text
	if state == "won" then
		local endText = "found the chalice of everlife"
		print("\#0"..endText, flr(player.x/16)*128+64-#endText*2, flr(player.y/16)*128+40, 1)

		local start = "❎ to return to menu"
		print("\#0"..start, flr(player.x/16)*128+64-#start*2, flr(player.y/16)*128+50, 1)	
	end
end

function print_outline(_txt,_x,_y,_clr,_bclr,_thick)
    -- if we don't pass a background color set it to white
    if _bclr == nil then _bclr = 7 end
	if _thick == nil then _thick = 1 end

    -- draw the text with the outline color offsetted in each direction
    -- based from the original text position we want to draw.
    -- This will create a big blob of singular colors, which's outliens
    -- will perfectly match the printed text outline
    for x=-_thick,_thick do
        for y=-_thick,_thick do
            print(_txt, _x-x, _y-y, _bclr)
        end
    end

    -- draw the original text with the intended color in the middle
    -- of the blob, creating the outline effect
    print(_txt, _x, _y, _clr)
end

function draw_outline(myspr, x, y, clr, thickness, flip, scale)
    -- nil check for the color parameter
    if clr == nil then clr = 7 end
    if thickness == nil then thickness = 1 end
    if flip == nil then flip = false end
    if scale == nil then scale = 1 end
    
    -- set color palette to outline
	for i=1,15,1 do
		pal(i, clr)
	end
	
    -- handle black outline transparency issues
    if clr == 0 then
        palt(0, false)
    end

    -- draw the sprite 9 times by 1-1 offsets
    -- in each direction. the created blob is 
    -- which is the sprite's outline 
    for i=-thickness,thickness do
        for j=-thickness,thickness do
            spr(myspr, x-i, y-j, scale, scale, flip, false)
        end
    end
    
    -- reset black color transparency
    if clr == 0 then
        palt(0, true)
    end

    -- reset color palette
	pal()

    -- draw the original sprite in the middle
    -- which causes the outline effect
	spr(myspr, x, y,  scale, scale, flip, false)
end
__gfx__
00000000111111111111111100000000000000000011110000000000000000000000000000000000000000001100101100000000111111110000000000000000
00000000111111111111111100000000011111000001010000000000000000000000000000000000000000001000000000000000000000010000000000000000
00700700111111111111111100000000011010000011110000000000000000000000000000000000000000001010000000000000100000010000000000000000
00077000111111110000000000000000011000000000100000111100000000000000000000000000000000000010100000000000000010010000000000000000
00077000111111111101110100000000011111000011110001000010000000000000000000000000000000000010101000000000000101010000000000000000
00700700111111111101110100000000110001100100101001101010000000000000000000000000000000000010101000000000100111010000000000000000
00000000111111110000000000000000101110100001100001000010000000000000000000000000000000001000000000000000000000010000000000000000
00000000111111111111111100000000010001000010010001111110000000000000000000000000000000001101100100000000111111110000000000000000
00111100000000000100100000000000000000000000000000000000000000000000000000000000000000000000000000111100001111000000000000000000
00010100001111000111101001001000001001000000000001111000000000000000000000000000000000000000000010010100100101000000000000000000
00111100000101000011000101111010001111000010010001111010011110100111111000000000000000000000000010000000100000000000000000000000
00001000001111001111100100110001010101000011110000101010011110100100001001111110000000000000000000110111001101110000000000000000
00111100000010001010101011111001011111000101010001111010001010100110101010000001000000000000000010110101101101010000000000000000
01001010010010101111101010101010000110000111110011000110011110100100001010101001001111000000000010010101100101010000000000000000
00011000000110000000000011111010011010100110101010110110110001100100001010000001000101000011110010000010100000100000000000000000
00100100001001000100010001000100111101111111011101001000010010000111111011111111001111000101011010100100101001000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000110011000001000000000000011100000111100000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000101101000001000000000000010100001000010000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000100001000011100000000000011100001111110000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000111111000001000011110000001000000111100000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000101101000010100001110000011000000011000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000010010000001000011101000001000000011000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000001100000001000001111000011000000111100000000000000000000000000
00000000000000000000000000000000000000000000000011001011111111110000000000000000000000000000000000000000000000000000000000000000
01111100011111000111110000000000000000000000000010000000000000010110010001100000000000000000000000000000011001100000000000000000
01101000011010100110101000000000000000000000000010100000100000010110000001100000000001000000000000000000011001100000000000000000
00000000011000100000001000000000000000000000000000101000000010010000000000000000000000000001000000111000000000000000000000000000
11101100011110101110101000000000000000000000000000101010000101010000000000000000000100000000100001000100000000000000000000000000
11100110110001111110011100000000000000000000000000101010100111010100011000100000000000000010101000010000011001100000000000000000
11101010101110101110101000000000000000001100001010000000000000010000011000000010000000000000000000010010011001100000000000000000
01000100010001000100010000000000000000001101011111011001111111110000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111111110110000111000101000000010000000110100010100100001000000010000000
10000010100010001000001010000000000000010000000101000301000000010110000111100101000000010110111110101111101110301000001010111100
10300010103012001030000010200011010001011100000101020001000000010000000101200101003000010130000110000000100000001110000010000100
10000000100000001000000010002000000000010000030101000111003000010000100100030101000001010000000110300010102011101000200010030110
10000000100000001000001010000010000300010000020100000001000000210020100100000001001111010000200110000010100001001000000010200100
10002010101000001000011010000010000000010100000100000001000000011100030111110111000000010000000110020011101001001030010010000100
10010010101000001100111010101110010001010110010101111001000000010000100100000111000002010100000110000000100000001000010010000000
10000000101110001000000010100000000000010000010100001001000000011111111111111111111111111111111111111111111111111111111111111111
__label__
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddddddddddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhddddddddd
ddddddddddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddddddddddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhddddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhddddhhddddhhhhddhhddhhddddddhhddddhhhhddhhhhhhddddddhhddddhhhhhhddddhhhhhhhdhhhdhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhddddhhddddhhhhddhhddhhddddddhhddddhhhhddhhhhhhddddddhhddddhhhhhhddddhhhhhhhhhdhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhddhhhhhhddhhddhhddhhddhhddddddhhddddhhhhddhhhhhhhhddhhhhddhhddhhddhhhhhhhhhhhhhdhhdhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhddhhhhhhddhhddhhddhhddhhddddddhhddddhhhhddhhhhhhhhddhhhhddhhddhhddhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhdhhhhhhhddhhhhhhddddhhhhddhhddhhddhhddhhddhhddhhddhhhhhhhhddhhhhddhhddhhddhhddhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhdhhhhhhhddhhhhhhddddhhhhddhhddhhddhhddhhddhhddhhddhhhhhhhhddhhhhddhhddhhddhhddhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhdhhhhhhhhhddddhhddhhddhhhhddddhhddhhddhhddddddhhhhddddhhddddddhhddhhddhhddddddhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhdddhhhhhhhhddddhhddhhddhhhhddddhhddhhddhhddddddhhhhddddhhddddddhhddhhddhhddddddhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhdhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdhdddhdhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddhhddddhhhhddhhddhhhhddddhhddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddhhddddhhhhddhhddhhhhddddhhddddddhhhddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhhhhhddhhddhhddddddhhddhhddhhhhddhhhhdhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhhhhhddhhddhhddddddhhddhhddhhhhddhhhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhhhhhddddhhhhhhhhddhhddddddhhhhddhhhhdddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhhhhhddddhhhhhhhhddhhddddddhhhhddhhhhdhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddhhddhhddhhddddhhhhddhhhhhhhhddhhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddhhddhhddhhddddhhhhddhhhhhhhhddhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhdddhhddhdddhdddhdhhhdhhhdhdhhhhhdddhdddhdddhdddhdddhddhhhddhhhhhhddhdddhdddhdddhhhhhddhhdddhdddhhddhhhhhhhdddddddd
ddddddddhhhhhhhdhhdhdhhdhhdhdhdhhhdhhhdhdhhhhhdhdhdddhdhdhhhdhhdhhdhdhdhhhhhhhdhhhdhdhdddhdhhhhhhhdhdhdhhhdddhdhdhhhhhhhdddddddd
ddddddddhhhhhhhdhhdhdhhdhhdddhdhhhdhhhdddhhhhhdddhdhdhdddhhdhhhdhhdhdhdhhhhhhhdhhhdddhdhdhddhhhhhhdhdhddhhdhdhdhdhhhhhhhdddddddd
ddddddddhhhhhhhdhhdhdhhdhhdhdhdhhhdhhhhhdhhhhhdhdhdhdhdhdhdhhhhdhhdhdhdhdhhhhhdhdhdhdhdhdhdhhhhhhhdhdhdhhhdhdhdhdhhdhhhhdddddddd
ddddddddhhhhhhhdhhddhhhdhhdhdhdddhdddhdddhhhhhdhdhdhdhdhdhdddhdddhdhdhdddhhhhhdddhdhdhdhdhdddhhhhhdddhdddhdhdhddhhhhdhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdhdhdhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddddddddddhhhhhhhhhhhhhhhhddddddddddddddddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddddddddddhhhhhhhhhhhhhhhhdddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddddddddddhhhhhhhhhhhhhhhhdddddddddddhhdddddhhhhhhdddhhddhhhhhhddhdddhdddhdddhdddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhdhddhhhhhhdhhdhdhhhhhdhhhhdhhdhdhdhdhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
ddhdddhdddhdddhdhhhhhhhhhhhhhhhhddhdddhdddhhdddhdddhhhhhhdhhdhdhhhhhdddhhdhhdddhddhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhdddhd
ddhdddhdddhdddhdhhhhhhhhhhhhhhhhddhdddhdddhhddhdhddhhhhhhdhhdhdhhhhhhhdhhdhhdhdhdhdhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhdddhd
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddhhhhhhhdhhddhhhhhhddhhhdhhdhdhdhdhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
ddddddddddddddddhhhhhhhhhhhhhhhhdddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hddhhddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhddh
hddhhddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhddh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hddhhddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhddh
hddhhddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhddh
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hddhhddhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhddh
hddhhddhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhddh
hhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
hddhhddhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdhdhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhddh
hddhhddhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhddh
hhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
ddddddddhhhhhhhhddddddddddddddddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddddddddddhhhhhhhhdddddddddddddddddddddddddddddddd
ddddddddhhhhhhhhddddddddddddddddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddddddddddhhhhhhhhdddddddddddddddddddddddddddddddd
ddddddddhhhhhhhhddddddddddddddddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddddddddddhhhhhhhhdddddddddddddddddddddddddddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhddhdddhdddhdddhdddhdddhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddddhdddhdhhhhhhhhddhdddhdddhdddhdddhdddhddddddddd
ddddddddhhhhhhhhddhdddhdddhdddhdddhdddhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddddhdddhdhhhhhhhhddhdddhdddhdddhdddhdddhddddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhddddddddddddddddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddddddddddhhhhhhhhdddddddddddddddddddddddddddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhdddhdhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhdddhdhhhdhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhddddhhhhhhhhhhddddddddddddddddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhdhdhhhhhhhhhhddddddddddddddddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhddddhhhhhhhhhhddddddddddddddddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhdhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhddddhhhhhhhhhhddhdddhdddddddddddhdddhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhdhhdhdhhhhhhhhhddhdddhdddddddddddhdddhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhddhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhdhhdhhhhhhhhhhddddddddddddddddddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdhdhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdhhdhdhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdhhdhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhddddddddhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhddddddddhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhddddddddhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhddhdddhdhhhhhhhhhhhhhhhhddhdddhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhddhdddhdhhhhhhhhhhhhhhhhddhdddhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhddddddddhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhdhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
ddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhddddddddhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhdddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
ddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhd
ddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhdddhd
hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd

__gff__
0001010000000000000000000000000000000000000000000000000000000000000000000000000002020202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
010100000033000330003300030000300003000030000300003000030000300003000030000300003000030000300003000030000300003000030000300003000030000300003000030000300003000030000300
00010000195501a5401a5401a5401a5401b5401c5401d53022530275301f5201a520175201352011510105100f5100e5100e5100c5100a5100851006510045100351002510015100051000500005000050000500
0001000002350043500535006350083500f3500935005350033500235001350003000030000300003000030000300003000030000300003000030000300003000030000300003000030000300003000030000300
00010000234501f4501b4501845016450144501345013450004500b45000000014500000000450000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200000615006150061501015010150101500715007150101501015010150101500010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100
000200001615016150161501f1501f1501f1502815028150281501f1501f1501f1500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011000001072410722107221072210722107221072210725127241272212722127221272212722127221272515724157221572215722157221572215722157251372413722137221372213722137221372213725
0110000007524075220752207522075220752207522075250a5240a5220a5220a5220a5220a5220a5220a5250c5240c5220c5220c5220c5220c5220c5220c5250b5240b5220b5220b5220b5220b5220b5220b525
011000001072410722107221072210722107221072210725157241572215722157221572215722157221572513724137221372213722137221372213722137251172411722117221172211722117221172211725
0110000007524075220752207522075220752207522075250c5240c5220c5220c5220c5220c5220c5220c5250b5240b5220b5220b5220b5220b5220b5220b5250952409522095220952209522095220952209525
0110000010724107221072210722107221072210722107250e7240e7220e7220e7220e7220e7220e7220e7250c7240c7220c7220c7220c7220c7220c7220c7251072410722107221072210722107221072210725
011000000752407522075220752207522075220752207525055240552205522055220552205522055220552504524045220452204522045220452204522045250552405522055220552205522055220552205525
011000000c033000000000000000006300c0000c0000c0000c033000000000000000006300c0000c0000c0000c033000000000000000056300c0000c0000c0000c033000000000000000056300c0000c0000c000
011000000c033000000000000000006300c000006300c0000c033000000000000000006300c0000c0000c0000c033000000000000000056300c000056300c0000c033000000000000000056300c0000c0000c000
__music__
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
01 0a0b1044
00 0a0b1044
00 0c0d1144
02 0e0f1144
06 4a4b4344

