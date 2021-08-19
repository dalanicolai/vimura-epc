#!/usr/bin/env python
import fitz

from epc.server import EPCServer

server = EPCServer(('localhost', 0))

doc = None

@server.register_function
def vimura_init(doc_file):
    global doc
    doc = fitz.open(doc_file)
    return doc.metadata

@server.register_function
def test(page):
    print(page)
    page = doc[page]
    pix = page.get_pixmap()
    # mag = display_width / pix.width
    # svg = page.get_svg_image(matrix=fitz.Matrix(mag, mag))
    return pix.height

@server.register_function
def page_svg(page, display_width):
    page = doc[page]
    pix = page.get_pixmap()
    mag = display_width / pix.width
    svg = page.get_svg_image(matrix=fitz.Matrix(mag, mag))
    # svg = page.get_svg_image(matrix=fitz.Identity)
    return svg

server.print_port()
server.serve_forever()
