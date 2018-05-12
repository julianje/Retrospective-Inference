import BoundingBox
import random
import numpy as np
import Frame

class MetaModel:

	def __init__(self, detectionprob, illusionprob, illusionlambda, occlusionprob):
		self.detectionprob = detectionprob
		self.illusionprob = illusionprob
		self.illusionlambda = illusionlambda
		self.occlusionprob = occlusionprob

	def render(self, frame, boxsampler):
		"""
		takes a set of objects and renders the frame
		boxsapmler is a sampler of bounding boxes (from the prior)
		"""
		renderedboxes = []
		for i in frame.boundingboxes:
			if random.random() < self.detectionprob:
				renderedboxes.append(i)
		if random.random() < self.illusionprob:
			for j in range(len(np.random.poisson(self.illusionlambda))):
				renderedboxes.append(boxsampler.sample())
		return Frame.Frame(renderedboxes)
