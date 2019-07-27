require 'cairo'

cr = nil

function conky_pre()
   if conky_window == nil then
      return
   end
   local cs = cairo_xlib_surface_create(conky_window.display,
                                        conky_window.drawable,
                                        conky_window.visual,
                                        conky_window.width,
                                        conky_window.height)
   cr = cairo_create(cs)
   cairo_set_line_width(cr, 5)
   return ''
end

function conky_bar(line, start, length, r, g, b)
   cairo_rectangle(cr, 5 + (7 * 8) + start, 6 + (line * 16), length + 1, 13)
   cairo_set_source_rgba(cr, r/100, g/100, b/100, 0.5)
   cairo_fill (cr)
   return ''
end

function conky_tall_bar(line, start, length, r, g, b)
   cairo_rectangle(cr, 5 + (7 * 8) + start, 6 + (line * 16) - 2, length + 1, 16)
   cairo_set_source_rgba(cr, r/100, g/100, b/100, 0.5)
   cairo_fill (cr)
   return ''
end


function conky_post()
   cairo_destroy(cr)
   cairo_surface_destroy(cs)
   cr=nil
   return ''
end
