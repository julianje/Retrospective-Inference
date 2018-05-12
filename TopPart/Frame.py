class Frame:
	"""
	Intermediate representation that can be produced
	by the net, or by a rendering function
	"""
	def __init__(self, boundingboxes):
		self.boundingboxes = boundingboxes