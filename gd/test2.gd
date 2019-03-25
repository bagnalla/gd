extends Control

var textures = []
var max_x
var max_y
var size
var r
var g
var b
var a

var field = []
const STARTOTAL := 600

const SIZE_INDEX = 0
const POSITION_INDEX = 1
const COLOR_INDEX = 2

enum { TEST = 0+1 }

func TestFunc():
	print(TEST)

func yield():
	print("yield")

func Vector2():
	print('Vector2')

func _ready():
	var _asd_ = $ bakow
	TestFunc()
	self.yield()
	self.Vector2()
	textures.push_back(load("res://img/star_0.png"))
	textures.push_back(load("res://img/star_1.png"))
	textures.push_back(load("res://img/star_2.png"))
	max_x = get_viewport().size.x
	max_y = get_viewport().size.y
	create_star_field()

func _process(delta):
	update() #calls _draw()

func fix_saturation(brightness):
	if brightness < 0.15:
		return 0.15
	elif brightness > 0.85:
		return 0.85
	return brightness

func create_star_field():
	randomize()
	max_x = get_viewport().size.x
	max_y = get_viewport().size.y
	for i in range (1,STARTOTAL):
		size = 0
		if i % 5 == 0:
			size += 1
		if i % 50 == 0:
			size += 1
			
		if size == 1:
			r = randf() / 2
			g = randf() / 3
			b = randf() * 1.5
			if b > 1:
				b = 1
		
		if size == 2:
			r = fix_saturation(randf())
			g = fix_saturation(randf())
			b = fix_saturation(randf())
		
		a = (randf()/2) + 0.5
		var color = Color(r,g,b,a)
		var position = Vector3(gaussian(0,1), gaussian(0,1), gaussian(0,1)).normalized()
		field.push_back([size, position, color])

func gaussian(mean, deviation):
    var x1 = null
    var x2 = null
    var w = null
    while true:
        x1 = rand_range(0, 2) - 1
        x2 = rand_range(0, 2) - 1
        w = x1*x1 + x2*x2
        if 0 < w && w < 1:
            break
    w = sqrt(-2 * log(w)/w)
    return (mean + deviation * x1 * w)

#func _input(event):
#	if event is InputEventMouseMotion:
#		allow_draw = true

func _draw():
	max_x = get_viewport().size.x
	max_y = get_viewport().size.y
	
	# var cam_pos = Game.cam.global_transform.origin
	# var bounds = Rect2(-2, -2, max_x + 4, max_y + 4)
	
	# for star in field:
	# 	var world_point = cam_pos + star[POSITION_INDEX]
	# 	if Game.cam.is_position_behind(world_point):
	# 		var pos = Game.cam.unproject_position(world_point)
	# 		pos.x = round(pos.x)
	# 		pos.y = round(pos.y)
	# 		if bounds.has_point(pos):
	# 			draw_texture (textures[star[SIZE_INDEX]], pos, star[COLOR_INDEX])
