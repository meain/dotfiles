local capi = {screen=screen,client=client,mouse=mouse, keygrabber = keygrabber}
local math,table = math,table
local wibox        = require( "wibox"          )
local awful        = require( "awful"          )
local cairo        = require( "lgi"            ).cairo
local color        = require( "gears.color"    )
local beautiful    = require( "beautiful"      )
local surface      = require( "gears.surface"  )

local module = {}

local w = nil

function arrow_path(cr, width, sidesize)
  cr:rel_move_to( 0                   , -width/2 )
  cr:rel_line_to( width/2             , width/2  )
  cr:rel_line_to( -sidesize           , 0        )
  cr:rel_line_to( 0                   , width/2  )
  cr:rel_line_to( (-width)+2*sidesize , 0        )
  cr:rel_line_to( 0                   , -width/2 )
  cr:rel_line_to( -sidesize           , 0        )
  cr:rel_line_to( width/2             , -width/2 )
  cr:close_path()
end

function module.highlight()
  if not w then
    w = wibox{}
    w.height = 50
    w.width = 50
    w.ontop = true

    img = cairo.ImageSurface(cairo.Format.ARGB32, w.height,w.width)
    cr = cairo.Context(img)
    cr:set_source(color(beautiful.bg_urgent))
    cr:paint()
    -- cr:set_source(color(beautiful.fg_normal))
    -- cr:arc(50,50,50-3,0,2*math.pi)
    -- cr:set_line_width(5)
    -- cr:stroke()
    w:set_bg(cairo.Pattern.create_for_surface(img))
  end
  w.x = capi.mouse.coords().x -w.width/2
  w.y = capi.mouse.coords().y -w.height/2
  w.visible = true
end

function module.hide()
  if w then
    w.visible = false
  end
end

return module
