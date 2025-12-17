// Global state
const state = {
    parsedData: null,
    wellMappings: null,
    standardsData: null,
    curveModel: null,
    rSquared: null,
    finalResults: null,
    selectedRows: new Set()
};

// Parse a single PicoGreen file
function parsePicogreenFile(filename, content) {
    const lines = content.split('\n');
    const data = [];

    // Extract metadata
    const softwareVersionLine = lines.find(l => l.includes('Software Version'));
    const plateNumberLine = lines.find(l => l.includes('Plate Number'));
    const dateLine = lines.find(l => l.match(/^Date/));
    const timeLine = lines.find(l => l.match(/^Time/));

    const plateNumber = plateNumberLine ? plateNumberLine.match(/Plate \d+/)?.[0] : 'Plate 1';
    const date = dateLine ? dateLine.match(/\d{2}\/\d{2}\/\d{4}/)?.[0] : '';
    const time = timeLine ? timeLine.match(/\d{2}:\d{2}:\d{2}/)?.[0] : '';
    const dateTime = `${date} ${time}`;

    // Find results section
    let resultsStart = lines.findIndex(l => l.match(/^Results/));
    if (resultsStart === -1) {
        resultsStart = lines.findIndex(l => l.match(/^\s*1\s+2\s+3/));
    }

    // Parse 96-well plate data
    const rowLetters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'];

    rowLetters.forEach((rowLetter, i) => {
        const rowLineIdx = resultsStart + i + 1;
        if (rowLineIdx < lines.length) {
            const rowData = lines[rowLineIdx].trim().split(/\s+/);
            if (rowData.length >= 13 && rowData[0] === rowLetter) {
                for (let j = 1; j <= 12; j++) {
                    const value = parseFloat(rowData[j]);
                    if (!isNaN(value)) {
                        data.push({
                            File: filename,
                            Plate: plateNumber,
                            DateTime: dateTime,
                            Row: rowLetter,
                            Column: j,
                            Well: `${rowLetter}${j}`,
                            Fluorescence: value
                        });
                    }
                }
            }
        }
    });

    return data;
}

// Tab navigation
document.querySelectorAll('.tab-button').forEach(button => {
    button.addEventListener('click', () => {
        const tabName = button.dataset.tab;

        // Update active button
        document.querySelectorAll('.tab-button').forEach(b => b.classList.remove('active'));
        button.classList.add('active');

        // Update active content
        document.querySelectorAll('.tab-content').forEach(c => c.classList.remove('active'));
        document.getElementById(tabName).classList.add('active');
    });
});

// File Import Tab
document.getElementById('loadFiles').addEventListener('click', async () => {
    const fileInput = document.getElementById('fileInput');
    const files = fileInput.files;

    if (files.length === 0) {
        alert('Please select files first');
        return;
    }

    let allData = [];

    for (let i = 0; i < files.length; i++) {
        const file = files[i];
        const content = await file.text();
        const fileData = parsePicogreenFile(file.name, content);
        allData = allData.concat(fileData);
    }

    state.parsedData = allData;

    // Update file status
    document.getElementById('fileStatus').textContent =
        `Files processed: ${files.length}\nTotal wells: ${allData.length}\nPlates: ${new Set(allData.map(d => d.Plate)).size}`;

    // Display parsed data table
    displayParsedDataTable();

    // Create heatmap
    createPlateHeatmap();

    // Initialize well mappings
    initializeWellMappings();

    alert('Files loaded successfully!');
});

function displayParsedDataTable() {
    const container = document.getElementById('parsedDataTable');
    const html = createTableHTML(state.parsedData, ['File', 'Plate', 'DateTime', 'Row', 'Column', 'Well', 'Fluorescence']);
    container.innerHTML = html;
}

function createTableHTML(data, columns, selectable = false, editable = false) {
    if (!data || data.length === 0) return '<p>No data available</p>';

    let html = '<table><thead><tr>';
    if (selectable) html += '<th></th>';
    columns.forEach(col => html += `<th>${col}</th>`);
    html += '</tr></thead><tbody>';

    data.forEach((row, i) => {
        html += `<tr data-index="${i}">`;
        if (selectable) {
            html += `<td><input type="checkbox" class="row-select" data-index="${i}"></td>`;
        }
        columns.forEach(col => {
            const value = row[col] !== undefined ? row[col] : '';
            if (editable && col !== 'File' && col !== 'Plate' && col !== 'Well') {
                html += `<td contenteditable="true" data-col="${col}" data-row="${i}">${value}</td>`;
            } else {
                html += `<td>${value}</td>`;
            }
        });
        html += '</tr>';
    });

    html += '</tbody></table>';
    return html;
}

function createPlateHeatmap() {
    if (!state.parsedData) return;

    // Use first file/plate for visualization
    const firstFile = state.parsedData[0].File;
    const plotData = state.parsedData.filter(d => d.File === firstFile);

    const z = [];
    const text = [];
    const rowLetters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'];

    // Create 8x12 matrix
    for (let i = 0; i < 8; i++) {
        z[i] = [];
        text[i] = [];
        for (let j = 0; j < 12; j++) {
            const well = plotData.find(d => d.Row === rowLetters[i] && d.Column === j + 1);
            z[i][j] = well ? well.Fluorescence : 0;
            text[i][j] = well ? `Well: ${well.Well}<br>Fluorescence: ${well.Fluorescence}` : '';
        }
    }

    const data = [{
        z: z,
        x: Array.from({length: 12}, (_, i) => i + 1),
        y: rowLetters,
        type: 'heatmap',
        colorscale: 'Viridis',
        text: text,
        hovertemplate: '%{text}<extra></extra>'
    }];

    const layout = {
        title: '96-Well Plate Fluorescence Heatmap',
        xaxis: { title: 'Column', dtick: 1 },
        yaxis: { title: 'Row', autorange: 'reversed' },
        height: 400
    };

    Plotly.newPlot('plateHeatmap', data, layout);
}

// Download parsed data
document.getElementById('downloadParsed').addEventListener('click', () => {
    if (!state.parsedData) {
        alert('No data to download');
        return;
    }

    const ws = XLSX.utils.json_to_sheet(state.parsedData);
    const wb = XLSX.utils.book_new();
    XLSX.utils.book_append_sheet(wb, ws, 'Parsed Data');
    XLSX.writeFile(wb, `picogreen_parsed_${new Date().toISOString().split('T')[0]}.xlsx`);
});

// Well Mapping Tab
function initializeWellMappings() {
    if (!state.parsedData) return;

    // Create unique wells
    const uniqueWells = [];
    const seen = new Set();

    state.parsedData.forEach(d => {
        const key = `${d.File}-${d.Plate}-${d.Well}`;
        if (!seen.has(key)) {
            seen.add(key);
            uniqueWells.push({
                File: d.File,
                Plate: d.Plate,
                Well: d.Well,
                Type: 'Sample',
                Concentration_ng_uL: '',
                Units: 'ng/μL',
                Sample_ID: `Sample ${uniqueWells.length + 1}`
            });
        }
    });

    state.wellMappings = uniqueWells;
    displayMappingTable();
}

function displayMappingTable() {
    const container = document.getElementById('mappingTable');
    const columns = ['File', 'Plate', 'Well', 'Type', 'Concentration_ng_uL', 'Units', 'Sample_ID'];
    const html = createTableHTML(state.wellMappings, columns, true, true);
    container.innerHTML = html;

    // Add event listeners for selection
    document.querySelectorAll('.row-select').forEach(checkbox => {
        checkbox.addEventListener('change', (e) => {
            const index = parseInt(e.target.dataset.index);
            if (e.target.checked) {
                state.selectedRows.add(index);
            } else {
                state.selectedRows.delete(index);
            }
        });
    });

    // Add event listeners for editing
    document.querySelectorAll('#mappingTable td[contenteditable]').forEach(cell => {
        cell.addEventListener('blur', (e) => {
            const row = parseInt(e.target.dataset.row);
            const col = e.target.dataset.col;
            const value = e.target.textContent.trim();

            if (state.wellMappings[row]) {
                state.wellMappings[row][col] = value;
            }
        });
    });
}

// Update dilution calculations
function updateDilutionCalculations() {
    const sampleVol = parseFloat(document.getElementById('sampleVolume').value);
    const bufferVol = parseFloat(document.getElementById('bufferVolume').value);
    const reagentVol = parseFloat(document.getElementById('reagentVolume').value);

    const total = sampleVol + bufferVol + reagentVol;
    const dilutionFactor = total / sampleVol;

    document.getElementById('totalVolume').textContent = `${total} μL`;
    document.getElementById('dilutionFactor').textContent = `${dilutionFactor.toFixed(1)}x`;
}

document.getElementById('sampleVolume').addEventListener('input', updateDilutionCalculations);
document.getElementById('bufferVolume').addEventListener('input', updateDilutionCalculations);
document.getElementById('reagentVolume').addEventListener('input', updateDilutionCalculations);

// Show/hide standard inputs
document.getElementById('quickType').addEventListener('change', (e) => {
    document.getElementById('standardInputs').style.display =
        e.target.value === 'Standard' ? 'block' : 'none';
});

// Apply quick edit
document.getElementById('applyQuick').addEventListener('click', () => {
    if (state.selectedRows.size === 0) {
        alert('Please select rows first');
        return;
    }

    const type = document.getElementById('quickType').value;
    const conc = parseFloat(document.getElementById('standardConc').value);
    const units = document.getElementById('standardUnits').value === 'ng_mL' ? 'ng/mL' : 'ng/μL';

    state.selectedRows.forEach(index => {
        if (state.wellMappings[index]) {
            state.wellMappings[index].Type = type;
            if (type === 'Standard') {
                state.wellMappings[index].Concentration_ng_uL = conc;
                state.wellMappings[index].Units = units;
            } else {
                state.wellMappings[index].Concentration_ng_uL = '';
                state.wellMappings[index].Units = 'ng/μL';
            }
        }
    });

    displayMappingTable();
    state.selectedRows.clear();
    alert(`Updated ${state.selectedRows.size} wells`);
});

// Reset mappings
document.getElementById('resetMapping').addEventListener('click', () => {
    if (confirm('Are you sure you want to reset all mappings?')) {
        initializeWellMappings();
        alert('Mappings reset');
    }
});

// Download mapping
document.getElementById('downloadMapping').addEventListener('click', () => {
    if (!state.wellMappings) {
        alert('No mapping data to download');
        return;
    }

    const ws = XLSX.utils.json_to_sheet(state.wellMappings);
    const wb = XLSX.utils.book_new();
    XLSX.utils.book_append_sheet(wb, ws, 'Mapping');
    XLSX.writeFile(wb, `well_mapping_${new Date().toISOString().split('T')[0]}.xlsx`);
});

// Load mapping
document.getElementById('loadMapping').addEventListener('click', async () => {
    const fileInput = document.getElementById('mappingFile');
    const file = fileInput.files[0];

    if (!file) {
        alert('Please select a mapping file');
        return;
    }

    const data = await file.arrayBuffer();
    const workbook = XLSX.read(data);
    const worksheet = workbook.Sheets[workbook.SheetNames[0]];
    const jsonData = XLSX.utils.sheet_to_json(worksheet);

    state.wellMappings = jsonData;
    displayMappingTable();
    alert('Mapping loaded successfully!');
});

// Standard Curve Tab
document.getElementById('generateCurve').addEventListener('click', () => {
    if (!state.parsedData || !state.wellMappings) {
        alert('Please load data and mappings first');
        return;
    }

    // Merge data with mappings
    const merged = state.parsedData.map(d => {
        const mapping = state.wellMappings.find(m =>
            m.File === d.File && m.Plate === d.Plate && m.Well === d.Well
        );
        return { ...d, ...mapping };
    });

    // Filter for standards
    const standards = merged.filter(d =>
        d.Type === 'Standard' && d.Concentration_ng_uL !== '' && d.Concentration_ng_uL !== undefined
    );

    if (standards.length === 0) {
        alert('No standards found. Please map some wells as standards first.');
        return;
    }

    // Group by concentration and calculate means
    const grouped = {};
    standards.forEach(s => {
        // Convert ng/mL to ng/μL if needed
        const conc = s.Units === 'ng/mL' ?
            parseFloat(s.Concentration_ng_uL) / 1000 :
            parseFloat(s.Concentration_ng_uL);

        if (!grouped[conc]) {
            grouped[conc] = {
                values: [],
                originalConc: s.Concentration_ng_uL,
                originalUnits: s.Units
            };
        }
        grouped[conc].values.push(s.Fluorescence);
    });

    // Calculate statistics
    const standardsData = Object.keys(grouped).map(conc => {
        const values = grouped[conc].values;
        const mean = values.reduce((a, b) => a + b, 0) / values.length;
        const variance = values.reduce((a, b) => a + Math.pow(b - mean, 2), 0) / values.length;
        const sd = Math.sqrt(variance);
        const se = sd / Math.sqrt(values.length);

        return {
            Concentration_ng_uL: parseFloat(conc),
            Mean_Fluorescence: mean,
            SD_Fluorescence: sd,
            SE_Fluorescence: se,
            N: values.length,
            Original_Conc: grouped[conc].originalConc,
            Original_Units: grouped[conc].originalUnits
        };
    }).sort((a, b) => a.Concentration_ng_uL - b.Concentration_ng_uL);

    state.standardsData = standardsData;

    // Fit curve
    const curveType = document.getElementById('curveType').value;
    const dataPoints = standardsData.map(s => [s.Concentration_ng_uL, s.Mean_Fluorescence]);

    let result;
    if (curveType === 'linear') {
        result = regression.linear(dataPoints);
    } else if (curveType === 'poly2') {
        result = regression.polynomial(dataPoints, { order: 2 });
    } else if (curveType === 'poly3') {
        result = regression.polynomial(dataPoints, { order: 3 });
    }

    state.curveModel = result;
    state.rSquared = result.r2;

    // Display statistics
    document.getElementById('curveStats').textContent =
        `R-squared: ${result.r2.toFixed(4)}\nCurve Type: ${curveType}\nStandard Points: ${standardsData.length}`;

    // Display standards table
    displayStandardsTable();

    // Plot curve
    plotStandardCurve();

    alert('Standard curve generated!');
});

function displayStandardsTable() {
    const container = document.getElementById('standardsTable');

    const displayData = state.standardsData.map(s => ({
        'Original Concentration': `${s.Original_Conc} ${s.Original_Units}`,
        'Concentration (ng/μL)': s.Concentration_ng_uL.toFixed(4),
        'Mean_Fluorescence': s.Mean_Fluorescence.toFixed(2),
        'SD_Fluorescence': s.SD_Fluorescence.toFixed(2),
        'SE_Fluorescence': s.SE_Fluorescence.toFixed(2),
        'N': s.N
    }));

    const html = createTableHTML(displayData,
        ['Original Concentration', 'Concentration (ng/μL)', 'Mean_Fluorescence', 'SD_Fluorescence', 'SE_Fluorescence', 'N']
    );
    container.innerHTML = html;
}

function plotStandardCurve() {
    const standards = state.standardsData;

    // Plot points with error bars
    const trace1 = {
        x: standards.map(s => s.Concentration_ng_uL),
        y: standards.map(s => s.Mean_Fluorescence),
        error_y: {
            type: 'data',
            array: standards.map(s => s.SE_Fluorescence),
            visible: true
        },
        mode: 'markers',
        name: 'Standards',
        marker: { size: 10, color: 'blue' },
        type: 'scatter'
    };

    // Plot fitted curve
    const minConc = Math.min(...standards.map(s => s.Concentration_ng_uL));
    const maxConc = Math.max(...standards.map(s => s.Concentration_ng_uL));
    const concRange = [];
    for (let i = 0; i <= 100; i++) {
        concRange.push(minConc + (maxConc - minConc) * i / 100);
    }

    const fittedY = concRange.map(x => state.curveModel.predict(x)[1]);

    const trace2 = {
        x: concRange,
        y: fittedY,
        mode: 'lines',
        name: 'Fitted Curve',
        line: { color: 'red', width: 2 },
        type: 'scatter'
    };

    const data = [trace1, trace2];

    const layout = {
        title: 'Standard Curve',
        xaxis: { title: 'Concentration (ng/μL)' },
        yaxis: { title: 'Fluorescence' },
        height: 500
    };

    Plotly.newPlot('standardCurvePlot', data, layout);
}

// Results Tab
document.getElementById('calculateConcs').addEventListener('click', () => {
    if (!state.curveModel || !state.parsedData || !state.wellMappings) {
        alert('Please generate a standard curve first');
        return;
    }

    // Merge data with mappings
    const merged = state.parsedData.map(d => {
        const mapping = state.wellMappings.find(m =>
            m.File === d.File && m.Plate === d.Plate && m.Well === d.Well
        );
        return { ...d, ...mapping };
    });

    // Filter for samples
    const samples = merged.filter(d => d.Type === 'Sample');

    // Get dilution factor
    const sampleVol = parseFloat(document.getElementById('sampleVolume').value);
    const bufferVol = parseFloat(document.getElementById('bufferVolume').value);
    const reagentVol = parseFloat(document.getElementById('reagentVolume').value);
    const totalVol = sampleVol + bufferVol + reagentVol;
    const dilutionFactor = totalVol / sampleVol;

    const detectionLimit = parseFloat(document.getElementById('detectionLimit').value);
    const maxStandardFluor = Math.max(...state.standardsData.map(s => s.Mean_Fluorescence));

    // Calculate concentrations
    const results = samples.map(s => {
        let concInWell;

        // Solve for concentration from fluorescence
        const curveType = document.getElementById('curveType').value;
        if (curveType === 'linear') {
            // For linear: y = mx + b, solve for x: x = (y - b) / m
            const [b, m] = state.curveModel.equation;
            concInWell = (s.Fluorescence - b) / m;
        } else {
            // For polynomial, use numerical search
            const minConc = Math.min(...state.standardsData.map(d => d.Concentration_ng_uL));
            const maxConc = Math.max(...state.standardsData.map(d => d.Concentration_ng_uL));

            let bestConc = 0;
            let bestDiff = Infinity;

            for (let c = minConc; c <= maxConc; c += (maxConc - minConc) / 1000) {
                const predictedFluor = state.curveModel.predict(c)[1];
                const diff = Math.abs(predictedFluor - s.Fluorescence);
                if (diff < bestDiff) {
                    bestDiff = diff;
                    bestConc = c;
                }
            }
            concInWell = bestConc;
        }

        // Ensure non-negative
        concInWell = Math.max(0, concInWell);

        const originalConc = concInWell * dilutionFactor;

        let qcFlag = 'OK';
        if (s.Fluorescence < detectionLimit) {
            qcFlag = 'Below Detection Limit';
        } else if (s.Fluorescence > maxStandardFluor) {
            qcFlag = 'Above Curve Range';
        }

        return {
            File: s.File,
            Plate: s.Plate,
            Well: s.Well,
            Sample_ID: s.Sample_ID,
            Fluorescence: s.Fluorescence,
            Conc_in_well: concInWell,
            Original_Concentration_ng_uL: originalConc,
            QC_Flag: qcFlag
        };
    });

    state.finalResults = results;

    // Display results
    displayResultsTable();
    displaySummaryStats();

    alert('Concentrations calculated!');
});

function displayResultsTable() {
    const container = document.getElementById('resultsTable');

    const displayData = state.finalResults.map(r => ({
        ...r,
        Conc_in_well: r.Conc_in_well.toFixed(2),
        Original_Concentration_ng_uL: r.Original_Concentration_ng_uL.toFixed(2)
    }));

    const html = createTableHTML(displayData,
        ['File', 'Plate', 'Well', 'Sample_ID', 'Fluorescence', 'Conc_in_well', 'Original_Concentration_ng_uL', 'QC_Flag']
    );
    container.innerHTML = html;
}

function displaySummaryStats() {
    const results = state.finalResults;
    const okResults = results.filter(r => r.QC_Flag === 'OK');

    if (okResults.length > 0) {
        const concentrations = okResults.map(r => r.Original_Concentration_ng_uL);
        const mean = concentrations.reduce((a, b) => a + b, 0) / concentrations.length;
        const sorted = [...concentrations].sort((a, b) => a - b);
        const median = sorted[Math.floor(sorted.length / 2)];
        const min = Math.min(...concentrations);
        const max = Math.max(...concentrations);

        document.getElementById('summaryStats').textContent =
            `Total Samples: ${results.length}\n` +
            `Samples with QC = OK: ${okResults.length}\n` +
            `Below Detection Limit: ${results.filter(r => r.QC_Flag === 'Below Detection Limit').length}\n` +
            `Above Curve Range: ${results.filter(r => r.QC_Flag === 'Above Curve Range').length}\n` +
            `Mean Concentration (OK samples): ${mean.toFixed(2)} ng/μL\n` +
            `Median Concentration (OK samples): ${median.toFixed(2)} ng/μL\n` +
            `Range: ${min.toFixed(2)} - ${max.toFixed(2)} ng/μL`;
    } else {
        document.getElementById('summaryStats').textContent = 'No samples with OK QC status';
    }
}

// Download results
document.getElementById('downloadResults').addEventListener('click', () => {
    if (!state.finalResults) {
        alert('No results to download');
        return;
    }

    const wb = XLSX.utils.book_new();

    // Results sheet
    const ws1 = XLSX.utils.json_to_sheet(state.finalResults);
    XLSX.utils.book_append_sheet(wb, ws1, 'Results');

    // Standards sheet
    if (state.standardsData) {
        const ws2 = XLSX.utils.json_to_sheet(state.standardsData);
        XLSX.utils.book_append_sheet(wb, ws2, 'Standards');
    }

    // Settings sheet
    const sampleVol = parseFloat(document.getElementById('sampleVolume').value);
    const bufferVol = parseFloat(document.getElementById('bufferVolume').value);
    const reagentVol = parseFloat(document.getElementById('reagentVolume').value);
    const totalVol = sampleVol + bufferVol + reagentVol;
    const dilutionFactor = totalVol / sampleVol;

    const settings = [
        { Parameter: 'Sample Volume (μL)', Value: sampleVol },
        { Parameter: 'Buffer Volume (μL)', Value: bufferVol },
        { Parameter: 'PicoGreen Volume (μL)', Value: reagentVol },
        { Parameter: 'Total Volume (μL)', Value: totalVol },
        { Parameter: 'Dilution Factor', Value: dilutionFactor.toFixed(1) },
        { Parameter: 'Curve Type', Value: document.getElementById('curveType').value },
        { Parameter: 'R-squared', Value: state.rSquared ? state.rSquared.toFixed(4) : 'N/A' }
    ];
    const ws3 = XLSX.utils.json_to_sheet(settings);
    XLSX.utils.book_append_sheet(wb, ws3, 'Settings');

    XLSX.writeFile(wb, `picogreen_results_${new Date().toISOString().split('T')[0]}.xlsx`);
});
