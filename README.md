# PicoGreen Assay Analysis App

A browser-based application for analyzing PicoGreen fluorescence assay data. This tool allows you to upload raw fluorescence data, map wells to samples/standards, generate standard curves, and calculate sample concentrations.

## Features

- **File Import**: Parse PicoGreen assay files (.txt, .xpt)
- **Interactive Plate Heatmap**: Visualize fluorescence across 96-well plates
- **Well Mapping**: Assign well types (Sample, Standard, Empty) with custom concentrations
- **Standard Curve Generation**: Create linear or polynomial regression curves
- **Concentration Calculation**: Automatically calculate sample concentrations with dilution correction
- **Data Export**: Download parsed data, mappings, and results as Excel files
- **Quality Control**: Flag samples below detection limit or above curve range

## Deployment to GitHub Pages

### Step 1: Create a GitHub Repository

1. Go to [GitHub](https://github.com) and create a new repository
2. Name it something like `picogreen-analysis`
3. Make it public (required for GitHub Pages free hosting)
4. Don't initialize with README (we already have files)

### Step 2: Upload Files

You have two options:

#### Option A: Using Git Command Line

```bash
# Navigate to your project folder
cd "c:\Users\BonninM\OneDrive - CABI\Scripts\PicogreenAssay"

# Initialize git repository
git init

# Add all files
git add index.html app.js styles.css README.md

# Commit files
git commit -m "Initial commit: PicoGreen Analysis App"

# Add your GitHub repository as remote (replace with your username and repo name)
git remote add origin https://github.com/YOUR_USERNAME/picogreen-analysis.git

# Push to GitHub
git branch -M main
git push -u origin main
```

#### Option B: Using GitHub Web Interface

1. Go to your new repository on GitHub
2. Click "uploading an existing file"
3. Drag and drop these files:
   - `index.html`
   - `app.js`
   - `styles.css`
   - `README.md`
4. Click "Commit changes"

### Step 3: Enable GitHub Pages

1. In your repository, go to **Settings**
2. Scroll down to **Pages** in the left sidebar
3. Under "Source", select `main` branch
4. Click **Save**
5. Wait 1-2 minutes for deployment

Your app will be available at: `https://YOUR_USERNAME.github.io/picogreen-analysis/`

## How to Use the App

### 1. File Import Tab

1. Click "Choose PicoGreen Files" and select your .txt or .xpt files
2. Click "Load Files" to parse the data
3. View the parsed data table and plate heatmap
4. Download parsed data as Excel if needed

### 2. Well Mapping Tab

1. Set your dilution parameters:
   - Sample Volume (μL)
   - Buffer Volume (μL)
   - PicoGreen Volume (μL)

2. Map your wells:
   - Select rows in the table
   - Choose well type (Sample/Standard/Empty)
   - For standards, enter concentration and units (ng/μL or ng/mL)
   - Click "Apply to Selected Wells"

3. You can also:
   - Edit cells directly in the table
   - Import/Export mapping files (.xlsx)
   - Reset all mappings

### 3. Standard Curve Tab

1. Ensure you've mapped at least 2 standard concentrations
2. Select curve type:
   - Linear (recommended for most assays)
   - Polynomial 2nd order
   - Polynomial 3rd order
3. Click "Generate Standard Curve"
4. Review the R² value and curve plot
5. Check the standards table for mean fluorescence values

### 4. Results Tab

1. Set detection limit (minimum fluorescence threshold)
2. Click "Calculate Concentrations"
3. Review summary statistics
4. Check the results table for:
   - Concentration in well (ng/μL)
   - Original concentration (corrected for dilution)
   - QC flags
5. Download results as Excel file with multiple sheets:
   - Results
   - Standards
   - Settings

## File Format Requirements

The app expects PicoGreen output files with:
- Results section containing 8 rows (A-H) and 12 columns (1-12)
- Fluorescence values for each well
- Optional metadata (Software Version, Plate Number, Date, Time)

Example format:
```
Software Version: 6.3.0
Plate Number: Plate 1
Date: 12/17/2025
Time: 14:30:00

Results
   1      2      3      4      5      6      7      8      9      10     11     12
A  1234   2345   3456   4567   5678   6789   7890   8901   9012   1023   1134   1245
B  1111   2222   3333   4444   5555   6666   7777   8888   9999   1010   1111   1212
...
```

## Technical Details

### Technologies Used

- **HTML5**: Structure and layout
- **CSS3**: Styling and responsive design
- **Vanilla JavaScript**: Core functionality
- **Plotly.js**: Interactive charts and heatmaps
- **SheetJS (xlsx)**: Excel file import/export
- **PapaParse**: CSV parsing
- **regression.js**: Statistical curve fitting

### Browser Requirements

- Modern browser (Chrome, Firefox, Safari, Edge)
- JavaScript enabled
- Minimum screen width: 768px recommended

### Data Privacy

All data processing happens **entirely in your browser**. No data is sent to any server. Your files remain on your device.

## Troubleshooting

### App doesn't load on GitHub Pages

- Wait 2-3 minutes after enabling GitHub Pages
- Check that repository is public
- Verify all files (index.html, app.js, styles.css) are in the root directory

### Files won't upload

- Ensure files are .txt or .xpt format
- Check file format matches expected structure
- Try opening file in text editor to verify format

### Standard curve won't generate

- Verify you have at least 2 different standard concentrations
- Check that standards are properly mapped with concentrations
- Ensure concentration values are numeric

### Concentrations seem incorrect

- Verify dilution settings match your protocol
- Check that standard units are correct (ng/μL vs ng/mL)
- Review standard curve R² value (should be > 0.95)

## Support

For issues or questions:
1. Check the file format matches expected structure
2. Verify all steps are completed in order
3. Check browser console for error messages (F12)

## License

This tool is provided as-is for research use.
