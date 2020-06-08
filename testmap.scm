(define create-map
  (lambda ()

;ROOMS:
     ;tomb-entrance
    (define tomb-entrance
      (room 'tomb-entrance "This is a long hallway that fades into darkness." "You can barely hear the downtown bangor area."))

    ;tomb-hallway
    (define tomb-hallway
      (room 'tomb-hallway "This is another long hallway, though it's quite dark. You think that finding a way to brighten things up would be best." "You can hear mice scattering across the long hallway."))

    ;tomb-collapsed-room
    (define tomb-collapsed-room
      (room 'tomb-collapsed-room "This was once a large room now it's mostly caved in. Looks like an old storage room." "You can hear an erie silence."))

    ;tomb-scholar-room
    (define tomb-scholar-room
      (room 'tomb-scholar-room "This room is quite big, but cluttered with stacks of books and piles of old papers." "You can hear papers blowing around."))

    ;tomb-ladder-room
    (define tomb-ladder-room
      (room 'tomb-ladder-room "There is nothing in this room besides a ladder leading down." "You hear a faint sound movement."))

    ;tomb-roundtable-room
    (define tomb-roundtable-room
      (room 'tomb-roundtable-room "Unlike any other room you have seen thus far, this one is completely round with a giant table in the middle. You can see that the door to the north is locked, there might be a key to open it somewhere." "You hear an erie silence."))

    ;tomb-treasure-room
    (define tomb-treasure-room
      (room 'tomb-treasure-room "This room would probably be enourmous but there are piles upon piles of golden objects and coins. In the middle of all this gold you can see an enormous pedestal with dog footprints leading to it.  Whatever was there, your dog must have taken it." "You can hear the clanking of metal objects."))

    ;tomb-jail-room
    (define tomb-jail-room
      (room 'tomb-jail-room "Filled with doors to cells you think that it might not be best to linger in here." "You can hear a door creak open."))
    ;tomb-coffin-room
    (define tomb-coffin-room
      (room 'tomb-coffin-room "You immediately see large coffins and piles of bones everywhere. You start to get an erie feeling." "You hear a deathly silence."))

    ;tomb--dog-hideout
    (define tomb-dog-hideout
      (room 'tomb-dog-hideout "There is a stack of bones in this room, probably your dog's secret stash." "You can hear loud barking and loud movement."))

    ;tomb-eriks-room
    (define tomb-eriks-room
      (room 'tomb-eriks-room "You can see Erik's secret room for programming. Large whiteboards are on the walls filled with complex procedures.  You can see Erik very preoccupied with something on one of the whiteboards. There is a portal to the west, it looks like it leads to your classroom." "You hear someone typing in the distance."))

    ;tomb-class-room
    (define tomb-class-room
      (room 'tomb-class-room "You can seen the normal crowd of your peers staring at you. You're a tad bit late for class." "You can hear loud classroom noises."))

;OBJECTS:

    ;pen, helps the player completes the homework.
    (define pen (object 'pen 1 "Hmmm...This is a nice pen. It could come in use when you find that homework."))

    ;map, gives a description of the nearby rooms.
    (define map (object 'map 1 "Hmmm...This crumpled, ripped and old piece of paper has a few rooms on it, but you have no idea where they are."))

    ;torch, lights up the dark spaces.
    (define torch (object 'torch 5 "This is sure handy, things seem a lot brighter!"))

    ;stapler, staples the homework.
    (define stapler (object 'stapler 1 "Giant automatic stapler. This might come in handy when you find that homework."))

    ;key, opens a locked door
    (define key (object 'key 1 "WOW! Now you can unlock something!"))

    ;red-bull
    (define red-bull (object 'red-bull 1 "Positively alters your programming skills."))

    ;diamond
    (define diamond (object 'diamond 7 "WOW!!! This is rather large, it must be worth a lot!"))

    (connect-rooms! 'NS tomb-hallway tomb-entrance)
    (connect-rooms! 'EW tomb-scholar-room tomb-hallway)
    (connect-rooms! 'EW tomb-hallway tomb-collapsed-room)
    (connect-rooms! 'NS tomb-ladder-room tomb-hallway)
    (connect-rooms! 'NS tomb-roundtable-room tomb-ladder-room)
    (connect-rooms! 'EW tomb-roundtable-room tomb-treasure-room)
    (connect-rooms! 'EW tomb-jail-room tomb-roundtable-room)
    (connect-rooms! 'EW tomb-coffin-room tomb-jail-room)
    
    ;These rooms will be connected to the others when the use action of the key is evaluated.
    (connect-rooms! 'NS tomb-eriks-room tomb-dog-hideout)
    (connect-rooms! 'EW tomb-eriks-room tomb-class-room)
    
    (set-object-use-action! key 
       (lambda (obj plyr) 
         (connect-rooms! 'NS tomb-dog-hideout tomb-roundtable-room)))
    
    (add-object-to-room! torch tomb-collapsed-room)
    (add-object-to-room! map tomb-scholar-room)
    (add-object-to-room! pen tomb-jail-room)
    (add-object-to-room! key tomb-coffin-room)
    
    tomb-entrance))