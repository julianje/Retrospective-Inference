class BoundingBox:

	def __init__(self, x, y, x2, y2, label, confidence):
		self.x = x
		self.y = y
		self.x2 = x2
		self.y2 = y2
		self.label = label
		self.confidence = confidence