import java.util.LinkedList;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.Iterator;

public class Slideshow implements Iterable<Slide> {

	private static final int numberToTake = 350;

	private LinkedList<Slide> outputSlides;
	private ArrayList<Slide> inputSlides;

	public Slideshow(Image[] images) {
		inputSlides = new ArrayList<Slide>();
		outputSlides = new LinkedList<Slide>();
		
		boolean hasPrev = false;
		int prev = -1;

		for (int i = 0; i < images.length; i++) {
			if (images[i].isHorizontal) {
				inputSlides.add(new HSlide(images[i]));
			} else {
				if (hasPrev) {
					inputSlides.add(
						new VSlide(images[i], images[prev])
					);
					hasPrev = false;
				} else {
					prev = i;
					hasPrev = true;
				}
			}
		}

		while (!inputSlides.isEmpty()) {
			Slide slide = inputSlides.remove(0);

			Slide next = getNextSlide(slide);
			if (next == null) {
				// it was null because inputSlides was empty
				outputSlides.add(slide);
			} else {
				inputSlides.remove(next);
				outputSlides.add(slide);
				outputSlides.add(next);
			}
		}
	}

	private Slide getNextSlide(Slide slide) {
		return inputSlides
			.stream()
			.limit(numberToTake)
			.max((i, j) -> Integer.compare(i.getScoreWith(slide), j.getScoreWith(slide)))
			.orElse(null);
	}

	public Iterator<Slide> iterator() {
		return outputSlides.iterator();
	}

	public int getNumberOfSlides() {
		return outputSlides.size();
	}

}
