/* 
const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
console.log(urlParams);
const country = urlParams.get('iso');
console.log(country); */

 // settings

 const overviewURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRbvzBmXnsG40DyGmwKUEXvNRhdlNwWVCDTrbjpBoL1I6DTAbDZnN2Qu0p0OBE53KjpoyFkmYhrGdTW/pub?gid=361636222&single=true&output=csv&force=on";
 const samplingURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRbvzBmXnsG40DyGmwKUEXvNRhdlNwWVCDTrbjpBoL1I6DTAbDZnN2Qu0p0OBE53KjpoyFkmYhrGdTW/pub?gid=110833577&single=true&output=csv&force=on";
 const geosamplingURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRbvzBmXnsG40DyGmwKUEXvNRhdlNwWVCDTrbjpBoL1I6DTAbDZnN2Qu0p0OBE53KjpoyFkmYhrGdTW/pub?gid=1120370117&single=true&output=csv&force=on";
 
 
 
 const country = "Argentina";
 
 let CTI=[];
 let sampling=[];
 var CTIdata = [];
 var SamplingData = [];
 var GeoSamplingData = [];
 var mapData ;
 var totSampling ;
 // for ColorScale
 var colorScale ;
 
 $(document).ready(function() {
     function getData() {
         Promise.all([
             d3.csv(overviewURL),
             d3.csv(samplingURL),
             d3.json("../data/ARG2.geojson"),
             d3.csv(geosamplingURL),
         ]).then(function(data) {
             CTI=data[0];
             sampling=data[1];
             CTIdata = [parseInt(CTI[0]['Non-beneficiaries']), parseInt(CTI[0]['Beneficiaries']), parseInt(CTI[0]['Volunteers'])];
             console.log(CTIdata);
             SamplingData = [parseFloat(sampling[1]['Age1']), parseFloat(sampling[1]['Age2']), parseFloat(sampling[1]['Age3']), parseFloat(sampling[1]['Age4'])];
             totSampling = sampling[0]['Total_respondent'];
              // for ColorScale
             GeoSamplingData = data[3];
             console.log(GeoSamplingData);
             GeoSamplingValue = [];
             GeoSamplingData.forEach(element => {
              GeoSamplingValue.push(parseInt(element.Value))
              });
             colorScale = d3.scaleLinear()
             .domain([0, d3.max(GeoSamplingValue)])
             .range(["#ffcccc", "#FF0000"]);

             mapData = data[2];
             // Overview
             title(CTI);
             background(CTI);
             report(CTI); 
             generateRadialChart(CTIdata);
            
             // sampling
             figures(sampling);
             figFemales(sampling);
             figMales(sampling);
             limits(sampling)
             map(mapData);
         }); // then
        
     } // getData
     getData();
 });
 
 
 
 window.Apex = {
   chart: {
     foreColor: '#ccc',
     toolbar: {
       show: true
     },
   },
   stroke: {
     width: 3
   },
   dataLabels: {
     enabled: true
   },
   tooltip: {
     theme: 'dark'
   },
   grid: {
     borderColor: "#535A6C",
     xaxis: {
       lines: {
         show: true
       }
     }
   }
 };
 
 
  // Overview Radial Chart 
 
 function generateRadialChart(data){
     var optionsCircle4 = {
         chart: {
           type: 'radialBar',
           height: 350,
           width: '100%',
         },
         plotOptions: {
           radialBar: {
             size: undefined,
             inverseOrder: true,
             hollow: {
               margin: 10,
               size: '35%',
               background: 'transparent', //'#FFF', 
       
             },
             track: {
               show: false,
             },
             startAngle: -180,
             endAngle: 180
       
           },
         },
         stroke: {
           lineCap: 'round'
         },
         series: data,
         labels: ['Others', 'Beneficiaries', 'Volunteers'],
         colors: ['#EEEEEE', '#ff9999', '#FF0000'],
         legend: {
           inverseOrder: true,
           show: true,
           floating: true,
           position: 'bottom',
           offsetX: 0,
           offsetY: 0
         },
       }
       
       var chartCircle4 = new ApexCharts(document.querySelector('#radialBarBottom'), optionsCircle4);
       chartCircle4.render();
         
 }
 
 var optionsBar1 = {
   colors:['#bf0000', '#ff0000','#6B66B7','#090088','#d6d6d6'],
   chart: {
     height: 380,
     width:'100%',
     type: 'bar',
     stacked: true,
     stackType: '100%'
   },
   plotOptions: {
     bar: {
       columnWidth: '30%',
       horizontal: true,
     },
   },
 
   series: [{
     name: 'Not at all',
     data: [1, 1.2, 1.9, 1.5, 1.9, 1.7, 4.4, 2.3]
   }, {
     name: 'Not so much',
     data: [2.7, 4, 3.3, 2.5, 3.7, 3.7, 6.7, 3.7]
   }, {
     name: 'Mostly yes',
     data: [7.1, 8.3, 8.9, 10.2, 10.2, 11.0, 5.8, 11.9]
   }, {
     name: 'Yes completely',
     data: [85.9, 84.8, 84.0, 83.6, 81.3, 81.3, 79, 78.4]
   }, {
     name: 'Don’t know',
     data: [3.3, 1.7, 1.9, 2.3, 2.9, 2.3, 4.2, 3.7]
   }
 ],
   xaxis: {
     categories: ['Goodwill', 'Fairness', 'Inclusiveness', 'No discrimination', 'Participation', 'Integrity', 'Transparency','Neutrality'],
   },
 
   yaxis: {
    
     labels: {
      style: {
         colors: [],
         fontSize: '15px',
         fontFamily: 'Helvetica, Arial, sans-serif',
         fontWeight: 400,
         cssClass: 'apexcharts-yaxis-label',
         },
     },
   },
 
   fill: {
     opacity: 1
   },
   grid: {
     show: false,
     },
    title: {
         text: 'Perception of Value',
         align: 'center',
         margin: 10,
         offsetX: 50,
         style: {
             fontSize:  '14px',
             fontWeight:  'bold',
             fontFamily:  "Open Sans",
             color:  '#263238'
           },
     },
     legend: { 
       offsetX: 65,
     }
 }
 
 var chartBar1 = new ApexCharts(
   document.querySelector("#barchart-value"),
   optionsBar1
 );
 
 
 chartBar1.render();
 
 
 var optionsBar2 = {
     colors:['#bf0000', '#ff0000','#6B66B7','#090088','#d6d6d6'],
     chart: {
       height: 380,
       width:'100%',
       type: 'bar',
       stacked: true,
       stackType: '100%'
     },
     plotOptions: {
       bar: {
         columnWidth: '30%',
         horizontal: true,
       },
     },
   
     series: [{
       name: 'Not at all',
       data: [1, 1.2, 1.9, 1.5, 1.9, 1.7]
     }, {
       name: 'Not so much',
       data: [2.7, 4, 3.3, 2.5, 3.7, 3.7]
     }, {
       name: 'Mostly yes',
       data: [7.1, 8.3, 8.9, 10.2, 10.2, 11.0]
     }, {
       name: 'Yes completely',
       data: [85.9, 84.8, 84.0, 83.6, 81.3, 81.3]
     }, {
       name: 'Don’t know',
       data: [3.3, 1.7, 1.9, 2.3, 2.9, 2.3]
     }
   ],
     xaxis: {
       categories: ['Capability', 'Responsiveness', 'Knowledge', 'Approachability', 'Effectiveness', 'Relevance'],
     },
   
     yaxis: {
      
       labels: {
        style: {
           colors: [],
           fontSize: '15px',
           fontFamily: 'Helvetica, Arial, sans-serif',
           fontWeight: 400,
           cssClass: 'apexcharts-yaxis-label',
           },
       },
     },
   
     fill: {
       opacity: 1
     },
     grid: {
       show: false,
       },
      title: {
           text: 'Perception of Competencies',
           align: 'center',
           margin: 10,
           offsetX: 50,
           style: {
               fontSize:  '14px',
               fontWeight:  'bold',
               fontFamily:  "Open Sans",
               color:  '#263238'
             },
       },
       legend: { 
         offsetX: 65,
       }
   }
   
   var chartBar2 = new ApexCharts(
     document.querySelector("#barchart-comp"),
     optionsBar2
   );
   
   
   chartBar2.render();
 
 
  // Sampling charts
  // Age group
       
 var optionsDist = {
     series: [{
     data: [  11.08, 16.96, 12.95, 14.69], 
   },
   ],
     chart: {
     type: 'bar',
     height: 300,
     stacked: false,
     toolbar: {
       show: false
     }
   },
   colors: ['#FF0000'],
   plotOptions: {
     bar: {
       horizontal: false,
       barHeight: '80%',
     },
   },
   dataLabels: {
     enabled: true
   },
   stroke: {
     width: 3,
     colors: ["#12284C"]
   },
   
   grid: {
     xaxis: {
       lines: {
         show: false
       }
     },
     yaxis: {
       lines: {
         show: false
       }
     }
   },
   yaxis: {
   },
   tooltip: {
     shared: false,
     x: {
       formatter: function (val) {
         return "Age group:" + val
       }
     },
     y: {
       formatter: function (val) {
         return Math.abs(val) + "%"
       }
     }
   },
   title: {
   },
   xaxis: {
     categories: ['18-24','25-34', '35-44','45+' ],
     axisTicks: {
       show: false
   },
     
   },
   yaxis: {
     show:false,
   },
   
   };
 
   var chartDist = new ApexCharts(document.querySelector("#chartDist"), optionsDist);
   chartDist.render();
 
 function background() {
 
     var desc = CTI[0]['Background'];
           d3.select("#background").append("span")
         .text(desc); 
 
 
 };
 
 
 function title() {
     var title = country;
           d3.select("#title_country").append("span")
         .text(title); 
 };
 
 function report() {
 
   var rep = CTI[0]['Report'];
   if (rep != '') {
         d3.select("#report").append("span")
       .html('<a href="' + rep + '" ><div class="btn btn-primary btn-xl">Download Report</div></a>');
     }
 }; 
   
 function figures() {
 
     var respondents = sampling[0]['Total_respondent'];
           d3.select("#item-1").append("span")
         .text(respondents); 
 
 
 };
 
 function figFemales() {
 
   var females = sampling[0]['Female_respondent'];
         d3.select("#item-2").append("span")
       .text(females); 
 
 
 };
 function figMales() {
 
   var males = sampling[0]['Male_respondent'];
         d3.select("#item-3").append("span")
       .text(males); 
 
 
 };
 
 function limits() {
   var limitation = sampling[0]['Limitations'];
         d3.select("#text-limitations").append("span")
       .text(limitation); 
 };
 
 
 // SAMPLING MAP
 
 // Define the div for the tooltip
 var div = d3.select("#map_sampling").select("#tooltip")
     .attr("class", "tooltip")               
     .style("opacity", 0);
 
 // The svg
 var svg = d3.select("#map_sampling").select("svg").attr("width", 350).attr("height",400),
     width = +svg.attr("width"),
     height = +svg.attr("height");
 
 // Map and projection
 var projection = d3.geoMercator()
     .scale(500)
     .translate([700,-190])
 
     
 svg.style("border","10px");
 
 

 
 // Load external data and boot
 function map(data){
 
         // Draw the map
         svg.append("g")
             .selectAll("path")
             .data(data.features)
             .enter().append("path")
                 .attr("fill", function(d) {
 
                  ProvinceData = GeoSamplingData.filter(item => { return d.properties.nombre == item.Name; });//mettre les PCODE
                  var Val = ProvinceData.length != 0 ? parseInt(ProvinceData[0].Value) : 0;
                  console.log(Val)
                  return Val != 0 ? colorScale(Val) : "#CCC"; 
                  
                  //return colorScale(Val)
                 }) //"#737CA1"

                 .attr("stroke", "#FFF")
                 .attr("stroke-width", "1px")
                 .attr("d", d3.geoPath()
                     .projection(projection)
                 )
             .on("mouseover", function(d) { 
                ProvinceData = GeoSamplingData.filter(item => { return d.properties.nombre == item.Name; });
                var Val = ProvinceData.length != 0 ? parseInt(ProvinceData[0].Value) : 0;
                Tooltip = "<h6>" + d.properties.nombre + "</h6>" + Val; 
                 div.transition()        
                     .duration(200)      
                     .style("opacity", .9);      
                 div.html(Tooltip)
                     .style("left", (d3.event.pageX) + "px")  
                     .style("font-size", "0.8rem")
                     .style("padding", "5px")   
                     .style("top", (d3.event.pageY - 15) + "px"); 
                     })                  
                 .on("mouseout", function(d) { 
                     div.html(Tooltip).style("opacity", 0)
                 })
 
 }
 



 
 