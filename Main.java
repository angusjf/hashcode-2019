import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;

public class Main {

	public static void main(String[] args) {
		try {
			Scanner sc = new Scanner(new File(args[0]));
			
			Slideshow slideshow = new Slideshow(getImages(sc));

			System.out.println(slideshow.getNumberOfSlides());
			for (Slide s : slideshow) {
				System.out.println(s);
			}
		} catch (FileNotFoundException e) {
			System.err.println("file not found!");
		}
	}

	private static Image[] getImages(Scanner sc) {
		int numberOfImages = sc.nextInt();

		Image[] images = new Image[numberOfImages];

		for (int i = 0; i < numberOfImages; i++) {
			String hv = sc.next();
			boolean isHorizontal = hv.equals("H");
			int numberOfTags = sc.nextInt();
			List<String> tags = new ArrayList<>(numberOfTags);
			for (int j = 0; j < numberOfTags; j++) {
				tags.add(sc.next());
			}
			images[i] = new Image(isHorizontal, tags, i);
		}

		return images;
	}

}
