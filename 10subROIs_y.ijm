//V0.1. Takes a single ROI in entry, and divide it in 10 sub-ROIs along the y-axis. Then standard measurements are taken for each sub-ROI.
//eli.amson@smns-bw.de

Roi.getBounds(x, y, width, height);
print(height);
nR=floor(height)/10+1;
//width=Image.width();
//roiManager("Delete");

for (i = 0; i < 11; i++) {
	makeRectangle(x, floor(y)+i*nR, width, nR);
	roiManager("Add");
	}

for (i = 1; i < 11; i++) {
	roiManager("select", newArray(0, i));
	roiManager("AND");
	roiManager("Add");
	roiManager("Deselect");
}

roiManager("select", Array.getSequence(12));
roiManager("Delete");

for (i = 0; i < roiManager("count"); i++) {
roiManager("select", i);
run("Measure");
}
