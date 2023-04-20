const geodataUrl = 'https://community-trust-index.github.io/microsite/data/world.json';

const dataURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbooW7TmLrMZ8QNc4IlGq4mKaZQflviQ1WNPzeMHLemb8Nl5QdsDQnR5TnWHeNOzsFY479CV-tHbNY/pub?gid=0&single=true&output=csv&force=on";
const settingsURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbooW7TmLrMZ8QNc4IlGq4mKaZQflviQ1WNPzeMHLemb8Nl5QdsDQnR5TnWHeNOzsFY479CV-tHbNY/pub?gid=1974885344&single=true&output=csv&force=on";

let geomData,
    prioritiesData,
    settings;
let legendEntries = [];

let dataTable;

$(document).ready(function() {
    function getData() {
        Promise.all([
            d3.json(geodataUrl),
            d3.csv(dataURL),
            d3.csv(settingsURL),
        ]).then(function(data) {
            geomData = topojson.feature(data[0], data[0].objects.geom);

            prioritiesData = data[1];
            prioritiesData.forEach(element => {
                element.x = "0";
                element.y = "0";
                const stages = splitMultiValues(element['Stage']);
                stages.forEach(type => {
                    legendEntries.includes(type) ? null : legendEntries.push(type);
                });
            });

            prioritiesData = prioritiesData.filter(d => { return d.ISO3 != ''; });
            settings = data[2];

            //remove loader and show vis
            $('.loader').hide();
            $('#mainOfIframe').css('opacity', 1);

            figures();
            generateDataTable();
            initiateMap();
        }); // then
    } // getData

    getData();
});

//count countriesISO3Arr

function figures() {

    const count = { 'Test': 0, 'Engagement': 0, 'Ongoing': 0, 'Active': 0 };

    for (element of prioritiesData) {
        element['Stage'] == "Test" ? count["Test"] += 1 :
            element['Stage'] == "Engagement" ? count["Engagement"] += 1 :
            element['Stage'] == "Ongoing" ? count["Ongoing"] += 1 :
            element['Stage'] == "Active" ? count["Active"] += 1 : null;
    }
    var test = count['Test'];
    var engagement = count['Engagement'];
    var Ongoing = count['Ongoing'];
    var active = count['Active'];

    d3.select("#item-0").append("span")
        .text(test);

    d3.select("#item-1").append("span")
        .text(engagement);

    d3.select("#item-2").append("span")
        .text(Ongoing);

    d3.select("#item-3").append("span")
        .text(active);

}


function findOneValue(arrTest, arr) {
    return arr.some(function(v) {
        return arrTest.indexOf(v) >= 0;
    });
};

function splitMultiValues(arr, sep = ",") {
    const splitArr = arr.split(sep);
    var values = [];
    for (let index = 0; index < splitArr.length; index++) {
        values.push(splitArr[index]);
    }
    return values;
} //splitMultiValues

function updateLatLon(iso3, x, y) {
    for (let index = 0; index < prioritiesData.length; index++) {
        const element = prioritiesData[index];
        if (element.ISO3 == iso3) {
            element.x = x;
            element.y = y;
            break;
        }
    }

}

function stageClassName(stage) {
    var classe = 'btn tag-' + String(stage).trim();
    return '<label class="' + classe + '">' + stage + '</label>';
}

function getDataTableData(data = prioritiesData) {
    var dt = [];
    data.forEach(element => {
        if (element['Link'] != '') {
            dt.push(
                [element['ID'], 
                element['Country'], 
                stageClassName(element['Stage']), 
                element['Progress'], 
                element['Start Date'], 
                element['Contact'],
                '<a href="' + element['Link'] + '" target="blank">Download</a>'
                ]
            )
        }
        else {
            dt.push(
                [element['ID'], 
                element['Country'], 
                stageClassName(element['Stage']), 
                element['Progress'], 
                element['Start Date'], 
                element['Contact'],
                ''
                ]
            )
        }
    });
    return dt;
} //getDataTableData


function generateDataTable() {
    var dtData = getDataTableData();
    dataTable = $('#datatable').DataTable({
        data: dtData,
        "columns": [{
                "className": 'details-control',
                "orderable": false,
                "data": null,
                "defaultContent": '<i class="fa-solid fa-square-plus"></i>',
                "width": "1%"
            },
            { "width": "20%" },
            { "width": "15%" },
            { "width": "15%" },
            { "width": "15%" },
            { "width": "10%" },
            { "width": "10%" }
        ],
        "columnDefs": [{
                "className": "dt-head-left",
                "targets": "_all"
            },
            {
                "defaultContent": "-",
                "targets": "_all"
            },
        ],
        "autoWidth": false,
        "pageLength": 20,
        "bLengthChange": false,
        "pagingType": "simple_numbers",
        "order": [
            [0, 'asc']
        ],
        "dom": "Blrtp"
    });
} //generateDataTable

// search button
$('#searchInput').keyup(function() {
    dataTable.search($('#searchInput').val()).draw();
});


const isMobile = $(window).width() < 767 ? true : false;

const viewportWidth = window.innerWidth;
let currentZoom = 1;

const mapFillColor = '#596881', //00acee F9F871 294780 6077B5 001e3f A6B0C3
    mapInactive = '#596881', //1E3559
    mapActive = '#0e1c31', //A6B0C3
    hoverColor = '#546B89';

let g, mapsvg, projection, width, height, zoom, path, maptip;
let countriesISO3Arr = [];

function initiateMap() {
    width = document.getElementById("mainOfIframe").offsetWidth; //viewportWidth;
    height = (isMobile) ? 350 : 550;
    var mapScale = (isMobile) ? width / 5.5 : width / 7;
    var mapCenter = (isMobile) ? [12, 25] : [12, 25];
    projection = d3.geoMercator()
        .center(mapCenter)
        .scale(mapScale)
        .translate([width / 2.1, height / 2.1]);

    path = d3.geoPath().projection(projection);
    zoom = d3.zoom()
        .scaleExtent([1, 8])
        .on("zoom", zoomed);


    mapsvg = d3.select('#map').append("svg")
        .attr("width", width)
        .attr("height", height)
        .call(zoom)
        .on("wheel.zoom", null)
        .on("dblclick.zoom", null);

    mapsvg.append("rect")
        .attr("width", "100%")
        .attr("height", "100%")
        // .attr("fill", "#d9d9d9");
        .attr("fill", "#0e1c31"); //#1b365e //294780 //1b365e //cdd4d9
    // .attr("fill-opacity", "0.5");

    d3.select('#title').style('right', width / 2 + 'px');

    prioritiesData.forEach(element => {
        countriesISO3Arr.includes(element.ISO3) ? null : countriesISO3Arr.push(element.ISO3);
    });
    //map tooltips
    maptip = d3.select('#map').append('div').attr('class', 'd3-tip map-tip hidden');

    g = mapsvg.append("g"); //.attr('id', 'countries')
    g.selectAll("path")
        .data(geomData.features)
        .enter()
        .append("path")
        .attr('d', path)
        .attr('id', function(d) {
            return d.properties.ISO_A3;
        })
        .attr('class', function(d) {
            var className = (countriesISO3Arr.includes(d.properties.ISO_A3)) ? 'priority' : 'inactive';
            if (className == 'priority') {
                var centroid = path.centroid(d),
                    x = centroid[0],
                    y = centroid[1];
                updateLatLon(d.properties.ISO_A3, x, y);
            }
            return className;
        })
        .attr('fill', function(d) {
            return countriesISO3Arr.includes(d.properties.ISO_A3) ? mapFillColor : mapInactive;
        })
        .attr('stroke-width', 0.08)
        .attr('stroke', '#0e1c31')
        .on("mousemove", function(d) {
            countriesISO3Arr.includes(d.properties.ISO_A3) ? mousemove(d) : null;
        })
        .on("mouseout", function(d) {

            maptip.classed('hidden', true);
        });
    // .on("click", function(d) {
    //     if (countriesISO3Arr.includes(d.properties.ISO_A3)) {
    //         const countryInfo = prioritiesData.filter((c) => { return c.iso3 == d.properties.ISO_A3 })[0];
    //         generateEmergencyInfo(countryInfo);
    //     }

    // });

    const circlesR = (isMobile) ? 2 : 4;
    const circles = g.append("g")
        .attr("class", "cercles")
        .selectAll(".cercle")
        .data(prioritiesData)
        .enter()
        .append("g")
        .append("circle")
        .attr("class", "cercle")
        .attr("r", 7)
        //.attr("r", function(d) {
        //    const numIntervention = splitMultiValues(d["Intervention type"]).length;
        //    return circlesR * numIntervention;
        //})
        .attr("transform", function(d) {
            return "translate(" + [d.x, d.y] + ")";
        })
        // .attr("fill", '#f5333f')
        .attr("fill", function(d) {
            return getColor(d["Stage"]);
        })

    .attr("opacity", "0.9")
        .on("mousemove", function(d) {
            mousemove(d);
        })
        .on("mouseout", function() {
            maptip.classed('hidden', true);
        });

    // .on("click", function(d) {
    //     generateEmergencyInfo(d);
    //     g.selectAll("circle").attr('r', circlesR);
    //     $(this).attr('r', circlesR * 2);
    // });

    mapsvg.transition()
        .duration(750)
        .call(zoom.transform, d3.zoomIdentity);

    //zoom controls
    d3.select("#zoom_in").on("click", function() {
        zoom.scaleBy(mapsvg.transition().duration(500), 1.5);
    });
    d3.select("#zoom_out").on("click", function() {
        zoom.scaleBy(mapsvg.transition().duration(500), 0.5);
    });

    var legendSVG = d3.select('#legend').append("svg")
        .attr("widht", "100%")
        .attr("height", "100%");

    d3.select('#worldwide').style("bottom", height / 2 + "px");

    var worldwideSVG = d3.select('#worldwide').append("svg")
        .attr("widht", "100%")
        .attr("height", "100%");

    const xcoord = 10;
    legendSVG.append("g")
        .selectAll("legend-item")
        .data(legendEntries)
        .enter()
        .append("circle").attr("r", 6)
        .attr("cx", xcoord)
        .attr("cy", function(d, i) {
            if (i == 0) {
                return xcoord;
            }
            return xcoord + i * 25;
        })

    .attr("fill", function(legend) { return getColor(legend); });
    legendSVG
        .select("g")
        .selectAll("text")
        .data(legendEntries).enter()
        .append("text")
        .attr("x", xcoord * 2)
        .attr("x", xcoord * 2)
        .attr("y", function(d, i) {
            if (i == 0) {
                return xcoord + 5;
            }
            return xcoord + 5 + i * 25;
        })
        .text(function(d) { return d; });


} //initiateMap

function mousemove(d) {
    var html = '<div class="survole">';
    var countryName = "",
        stages,
        progresses;

    if (d.hasOwnProperty('properties')) {
        const arr = prioritiesData.filter((e) => { return e.ISO3 == d.properties.ISO_A3; });
        countryName = arr[0]["Country"];
        stages = splitMultiValues(arr[0]["Stage"]);
        progresses = splitMultiValues(arr[0]["Progress"]);

    } else {
        countryName = d["Country"];
        stages = splitMultiValues(d["Stage"]);
        progresses = splitMultiValues(d["Progress"]);
    }
    html += '<h6>' + countryName + '</h6>';
    for (let index = 0; index < stages.length; index++) {
        const stage = stages[index];
        html += '<button type="button" class="btn tag-' + stage + '">' + stage + '</button>';
    }
    html += '<div class="subtitle">Progress</div>';
    for (let index = 0; index < progresses.length; index++) {
        const agency = progresses[index];
        html += '<button type="button" class="btn tag-Project">' + progresses + '</button>';
    }

    html += '</div>'
    var mouse = d3.mouse(mapsvg.node()).map(function(d) { return parseInt(d); });
    maptip
        .classed('hidden', false)
        .attr('style', 'left:' + (mouse[0] + 5) + 'px; top:' + (mouse[1] + 10) + 'px')
        .html(html);
} //mousemove

// zoom on buttons click
function zoomed() {
    const { transform } = d3.event;
    currentZoom = transform.k;

    if (!isNaN(transform.k)) {
        g.attr("transform", transform);
        g.attr("stroke-width", 1 / transform.k);

        // updateCerclesMarkers()
    }
}


function getColor(type) {
    var color = '#cbd3d8';

    for (let index = 0; index < settings.length; index++) {
        const element = settings[index];
        if (element["Stage"] == type) {
            color = element["Legend Color"];
            break;
        }
        element["Stage"] == type ? color = element["Legend Color"] : null;

    }
    return color;
}

/* */

$(document).ready(function() {
    // $('.js-example-basic-single').select2();
});