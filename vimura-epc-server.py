import fitz

from epc.server import EPCServer

server = EPCServer(('localhost', 0))

@server.register_function
def test(page):
    doc = fitz.open("/home/dalanicolai/Downloads/Coleman-Coding-Freedom.pdf")
    page = doc[page]
    pix = page.get_pixmap()
    # mag = display_width / pix.width
    # svg = page.get_svg_image(matrix=fitz.Matrix(mag, mag))
    return pix.height

@server.register_function
def echo(page, display_width):
    doc = fitz.open("/home/dalanicolai/Downloads/Coleman-Coding-Freedom.pdf")
    page = doc[page]
    pix = page.get_pixmap()
    # mag = display_width / pix.width
    # svg = page.get_svg_image(matrix=fitz.Matrix(mag, mag))
    svg = page.get_svg_image(matrix=fitz.Identity)
    return svg

server.print_port()
server.serve_forever()
