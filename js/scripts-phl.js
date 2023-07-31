/* 
const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
console.log(urlParams);
const country = urlParams.get('iso');
console.log(country); */

 // settings

const overviewURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQYbWzFHRZW9gYnaMr7InGDJOQenFffk9wxKxmeeivTHxbnwh9_RDOjGdiXFYBqgk2bhNJdJZUaWn_G/pub?gid=361636222&single=true&output=csv&force=on";
const samplingURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQYbWzFHRZW9gYnaMr7InGDJOQenFffk9wxKxmeeivTHxbnwh9_RDOjGdiXFYBqgk2bhNJdJZUaWn_G/pub?gid=110833577&single=true&output=csv&force=on";
const geosamplingURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQYbWzFHRZW9gYnaMr7InGDJOQenFffk9wxKxmeeivTHxbnwh9_RDOjGdiXFYBqgk2bhNJdJZUaWn_G/pub?gid=1120370117&single=true&output=csv&force=on";
const chartCTI_url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQYbWzFHRZW9gYnaMr7InGDJOQenFffk9wxKxmeeivTHxbnwh9_RDOjGdiXFYBqgk2bhNJdJZUaWn_G/pub?gid=15486601&single=true&output=csv&force=on";


const country = "Philippines";

let CTI=[];
let sampling=[];
var Overall_Index = [];
var Volunteers_Index = [];
var Beneficiaries_Index = [];
var NonBeneficiaries_Index = [];
var CTIdata = [];
var SamplingData = [];
var SamplingAge_label = [];
var SamplingAge_Data = [];
var GeoSamplingData = [];
var chartCTIData = [];
var mapData ;
var totSampling ;
// for ColorScale
var colorScale ;

$(document).ready(function() {
    function getData() {
        Promise.all([
            d3.csv(overviewURL),
            d3.csv(samplingURL),
            d3.json("../data/PHL3.json"),
            d3.csv(geosamplingURL),
            d3.csv(chartCTI_url),
        ]).then(function(data) {
            CTI=data[0];
            sampling=data[1];
            Overall_Index = [parseFloat(CTI[0]['Index']).toFixed(0)]; 
            Volunteers_Index = [parseFloat(CTI[0]['Volunteers']).toFixed(0)];
            Beneficiaries_Index = [parseFloat(CTI[0]['Beneficiaries']).toFixed(0)];
            NonBeneficiaries_Index = [parseFloat(CTI[0]['Non-beneficiaries']).toFixed(0)];
            CTIdata = [parseFloat(CTI[0]['Non-beneficiaries']), parseFloat(CTI[0]['Beneficiaries']), parseFloat(CTI[0]['Volunteers'])];

             // Sampling data
             sampling=data[1];
             SamplingAge_label = [sampling[2]['Age1'], sampling[2]['Age2'], sampling[2]['Age3'], sampling[2]['Age4'], sampling[2]['Age5']];
             SamplingAge_Data = [parseFloat(sampling[1]['Age1']), parseFloat(sampling[1]['Age2']), parseFloat(sampling[1]['Age3']), parseFloat(sampling[1]['Age4']), parseFloat(sampling[1]['Age5'])];
 
            totSampling = sampling[0]['Total_respondent'];
            chartCTIData = data[4];
             var OverallComp = [];
             var beneficiariesComp= [];
             var volunteersComp = [];
             var othersComp = [];
             var driversComp= [];
             var OverallValue = [];
             var beneficiariesValue = [];
             var volunteersValue = [];
             var othersValue = [];
             var driversValue = [];
           
            chartCTIData.forEach(element => {
             if(element.Dimension == "Value"){
             OverallValue.push(element.Overall);
             beneficiariesValue.push(element.beneficiaries);
             volunteersValue.push(element.volunteers);
             othersValue.push(element.others);
             driversValue.push(element.Drivers);
             }
             if(element.Dimension == "Competency"){
               OverallComp.push(element.Overall);
               beneficiariesComp.push(element.beneficiaries);
               volunteersComp.push(element.volunteers);
               othersComp.push(element.others);
               driversComp.push(element.Drivers);
               }
            });

             // for ColorScale
            GeoSamplingData = data[3];
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
            coverage(CTI);
            lead(CTI);
            date(CTI);
            generateRadialChart(Overall_Index);
            generateRadial_Chart1(Volunteers_Index);             
            generateRadial_Chart2(Beneficiaries_Index);
            generateRadial_Chart3(NonBeneficiaries_Index);
            generateChartCTI (OverallComp, driversComp);
            generate_chartRadar2 (OverallValue, driversValue),
            
            // sampling
            figures(sampling);
            figFemales(sampling);
            figMales(sampling);
            limits(sampling)            
            samplingAge(SamplingAge_Data,SamplingAge_label);
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


 // Overall Radial Chart 

function generateRadialChart(data){
    var optionsCircle4 = {
        chart: {
          type: 'radialBar',
          height: 350,
          width: '100%',
          toolbar: {
            show: false
          },
        },
        
        plotOptions: {
          radialBar: {
            dataLabels: {
             name: {
               show: false,
               fontSize: '16px',
               fontFamily: undefined,
               fontWeight: 600,
               color: undefined,
               offsetY: -10
             },
             value: {
               show: true,
               fontSize: '36px',
               fontWeight: 600,
               color: undefined,
               offsetY: 16,
               formatter: function (val) {
               return val /10 + ''
             }
           }
           },
            inverseOrder: true,
            hollow: {
              margin: 10,
              size: '50%',
      
            },
            track: {
             show: false
         },
            
            startAngle: -0,
            endAngle: 360
      
          },
        },
        stroke: {
          lineCap: 'round'
        },
        series: data,
        labels: ['Index'],
        colors: ['#FF0000'],
        legend: {
          inverseOrder: true,
          show: false,
          floating: true,
          position: 'bottom',
          offsetX: 0,
          offsetY: 0
        },
        tooltip: {
         enabled: true,
         y: {
           show: true,
           formatter: function (val) {
             return  val /10 + ' out of 10'
           }
       }
       }
      }
      
      var chartCircle4 = new ApexCharts(document.querySelector('#radialBarBottom'), optionsCircle4);
      chartCircle4.render();
        
}


//


// Volunteer Radial Chart 

function generateRadial_Chart1(data){
 var optionsCircle1 = {
     chart: {
       type: 'radialBar',
       height: 250,
       width: '100%',
       toolbar: false,
     },
     plotOptions: {
       radialBar: {
         dataLabels: {
          name: {
            show: true,
            fontSize: '16px',
            fontFamily: undefined,
            fontWeight: 600,
            color: undefined,
            offsetY: -10
          },
          value: {
            show: true,
            fontSize: '36px',
            fontWeight: 600,
            color: '#FF0000',
            offsetY: 16,
            formatter: function (val) {
            return  val/10 + ''
          }
        }
        },
         inverseOrder: true,
         hollow: {
           margin: 10,
           size: '75%',
           background:'#eeeeee',
   
         },
         track: {
          show: true
      },
         
         startAngle: -0,
         endAngle: 360
   
       },
     },
     stroke: {
       lineCap: 'round'
     },
     
     series: data,
     labels: ['Score'],
     colors: ['#FF0000'],
     legend: {
       inverseOrder: true,
       show: false,
       floating: true,
       position: 'bottom',
       offsetX: 0,
       offsetY: 0
     },
     tooltip: {
      enabled: true,
      y: {
        show: true,
        formatter: function (val) {
          return  val /10 + ' out of10'
        }
    }
    }
   }
   
   var chartCircle1 = new ApexCharts(document.querySelector('#chartCTI_0'), optionsCircle1);
   chartCircle1.render();
     
}


// Beneficiaries Radial Chart 

function generateRadial_Chart2(data){
 var optionsCircle2 = {
     chart: {
       type: 'radialBar',
       height: 250,
       width: '100%',
       toolbar:false,
     },
     plotOptions: {
       radialBar: {
         dataLabels: {
          name: {
            show: true,
            fontSize: '16px',
            fontFamily: undefined,
            fontWeight: 600,
            color: undefined,
            offsetY: -10
          },
          value: {
            show: true,
            fontSize: '36px',
            fontWeight: 600,
            color: '#5F61B5',
            offsetY: 16,
            formatter: function (val) {
            return  val /10 + ''
          }
        }
        },
         inverseOrder: true,
         hollow: {
           margin: 10,
           size: '75%',
           background: '#eeeeee',
   
         },
         track: {
          show: true
      },
         
         startAngle: -0,
         endAngle: 360
   
       },
     },
     stroke: {
       lineCap: 'round'
     },
     series: data,
     labels: ['Score'],
     colors: ['#5F61B5'],
     legend: {
       inverseOrder: true,
       show: false,
       floating: true,
       position: 'bottom',
       offsetX: 0,
       offsetY: 0
     },
     tooltip: {
      enabled: true,
      y: {
        show: true,
        formatter: function (val) {
          return  val /10 + ' out of 10'
        }
    }
    }
   }
   
   var chartCircle2 = new ApexCharts(document.querySelector('#chartCTI_1'), optionsCircle2);
   chartCircle2.render();
     
}

// Other Radial Chart  

function generateRadial_Chart3(data){
 var optionsCircle3 = {
     chart: {
       type: 'radialBar',
       height: 250,
       width: '100%',
       toolbar: false,
     },
     plotOptions: {
       radialBar: {
         dataLabels: {
          name: {
            show: true,
            fontSize: '16px',
            fontFamily: undefined,
            fontWeight: 600,
            color: undefined,
            offsetY: -10
          },
          value: {
            show: true,
            fontSize: '36px',
            fontWeight: 600,
            color: '#18396C',
            offsetY: 16,
            formatter: function (val) {
            return  val /10 + ''
          }
        }
        },
         inverseOrder: true,
         hollow: {
           margin: 10,
           size: '75%',
           background: '#eeeeee',
   
         },
         track: {
          show: true,
          color: '#CCC',
      },
         
         startAngle: -0,
         endAngle: 360
   
       },
     },
     stroke: {
       lineCap: 'round'
     },
     series: data,
     labels: ['Score'],
     colors: ['#18396C'],
     legend: {
       inverseOrder: true,
       show: false,
       floating: true,
       position: 'bottom',
       offsetX: 0,
       offsetY: 0
     },
     tooltip: {
      enabled: true,
      colorScale: '#18396C',
      y: {
        show: true,
        formatter: function (val) {
          return  val /10 + ' out of 10'
        }
    }
    }
   }
   
   var chartCircle3 = new ApexCharts(document.querySelector('#chartCTI_2'), optionsCircle3);
   chartCircle3.render();
     
}


// chart1 - radial
  
  function generateChartCTI (OverallComp, driversComp){

  var optionsChartCTI = {
   
   colors: ['#FF0000', '#ff9999', '#CCCCCC'],
   series: [{
       name: 'Score' ,
       data: OverallComp, /* Overall data*/
     }
/*       , {
   name: 'Beneficiaries' ,
   data: beneficiariesArr 
 }, {
   name: 'Volunteers' ,
   data: volunteersArr 
 }, {
   name: 'Other' ,
   data: othersArr 
 } */
],
   chart: {
     height: 420,
     width:'100%',
     type: 'radar',
     toolbar:false
   },
 plotOptions: {
   bar: 
   {
     margin: 10,
     horizontal: true,
     barHeight:'60%',
     colors: {
       backgroundBarColors: ['#CCC'],
       backgroundBarOpacity: 1,
       backgroundBarRadius: 0,
   },
   dataLabels: {
     position: 'top',
   },
   }
 },
 dataLabels: {
   textAnchor: 'end',
   formatter: function (val) {
     return parseFloat(val/10).toFixed(1)
   },
   style: {
    fontSize: '14px',
    fontWeight: 'bold',
    colors: undefined
  },
  background: {
    enabled: true,
    foreColor: '#fff',
    padding: 4,
    borderRadius: 1,
    borderWidth: 1,
    borderColor: '#fff',
    opacity: 0.9,
  }
 },
 tooltip: {
   theme: 'dark',
    marker: {
     show: false,
   },
   x: {
     show: true
   },
   y: {

     formatter: function (val){
       return parseFloat(val/10).toFixed(8)
           }
   }
 },
 
 xaxis: {
   
   labels: {
   show: true,
   style: {
    fontSize: '14px',
  },
   },
   axisBorder: {
   show: false
   },
   axisTicks: {
   show: false
   },
   categories: driversComp, /* dimensions*/
 },
 yaxis: {
   show: true,
   labels: {
   show: false,
   formatter: function (val){
     return parseFloat(val/10)
         }
   },
   axisBorder: {
   show: false
   },
   axisTicks: {
   show: false
   }
 },
 };

 var ChartCTI = new ApexCharts(document.querySelector("#chartCTI"), optionsChartCTI);
 ChartCTI.render();
}

// chart2 - radar
function generate_chartRadar2 (OverallValue, driversValue){

var optionsRadar2 = {
 series: [{
 name: 'Score',
 data: OverallValue,
}],
 chart: {
   height: 410,
   width:'100%',
   type: 'radar',
   toolbar:false
},
plotOptions: {
 bar: 
 {
   margin: 10,
   horizontal: true,
   barHeight:'60%',
   colors: {
     backgroundBarColors: ['#CCC'],
     backgroundBarOpacity: 1,
     backgroundBarRadius: 0,
 },
 dataLabels: {
   position: 'top',
 
 },
 }
},

dataLabels: {
 textAnchor: 'end',
 formatter: function (val) {
   return parseFloat(val/10).toFixed(1)
 },
 style: {
  fontSize: '14px',
  fontWeight: 'bold',
  colors: undefined
},
background: {
  enabled: true,
  foreColor: '#fff',
  padding: 4,
  borderRadius: 1,
  borderWidth: 1,
  borderColor: '#fff',
  opacity: 0.9,
}
},
tooltip: {
  theme: 'dark',
   marker: {
    show: false,
  },
  x: {
    show: true
  },
  y: {

    formatter: function (val){
      return parseFloat(val/10).toFixed(8)
          }
  }
},
xaxis: {
   
  labels: {
  show: true,
  style: {
   fontSize: '14px',
 },
  },
  axisBorder: {
  show: false
  },
  axisTicks: {
  show: false
  },
  categories: driversValue, /* dimensions*/
},

yaxis: {
 show: true,
 labels: {
 show: false,
 formatter: function (val){
   return parseFloat(val/10)
       },
  
 },
 axisBorder: {
 show: false
 },
 axisTicks: {
 show: false
 }
},

};

var chartRadar2 = new ApexCharts(document.querySelector("#chartCTI2"), optionsRadar2);
chartRadar2.render();

}


 // Sampling charts
 // Age group
 function samplingAge(SamplingAge_Data, SamplingAge_label) {     
  var optionsDist = {
      series: [{
       name: 'Percentage',
       data: SamplingAge_Data,
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
      enabled: true,
      formatter: (val) => { return parseFloat(val).toFixed(1) + '%' }
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
          return "Age group: " + val
        }
      },
      y: {
       formatter: (val) => { return parseFloat(val).toFixed(1) + '%' }
      }
    },
    title: {
    },
    xaxis: {
      categories: SamplingAge_label,
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
  }

function background() {

    var desc = CTI[0]['Background'];
          d3.select("#background").append("span")
        .text(desc); 


};
function date() {
  var date = CTI[0]['Date'];
        d3.select("#text-date").append("span")
        .html('<b>Date</b>: '+ date); 
        console.log(date);
};

function lead() {

 var lead = CTI[0]['Lead'];
 var partners = CTI[0]['Partners'];
       d3.select("#lead").append("span")
       .html('<b>Lead</b>: '+ lead + '<br><b>Support</b>: ' + partners); 
};

function coverage() {

 var coverage = parseInt(CTI[0]['Coverage']);
       d3.select("#coverage").append("span")
       .html(''+ coverage + ' regions'); 
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
      .html('<a href="' + rep + '" ><div class="btn btn-primary btn-xl">See Analysis</div></a>');
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
    .scale(1150)
    .translate([-2370,450])

svg.style("border","0px");


function map(data){
console.log("test")
        svg.append("g")
            .selectAll("path")
            .data(data.features)
            .enter().append("path")
                .attr("fill", function(d) {

                 ProvinceData = GeoSamplingData.filter(item => { return d.properties.ADM1_PCODE == item.PCODE; });//mettre les PCODE
                 var Val = ProvinceData.length != 0 ? parseInt(ProvinceData[0].Value) : 0;
                 return Val != 0 ? colorScale(Val) : "#CCC"; 
                 
                 //return colorScale(Val)
                }) //"#737CA1"

                .attr("stroke", "#FFF")
                .attr("stroke-width", "1px")
                .attr("d", d3.geoPath()
                    .projection(projection)
                )
            .on("mouseover", function(d) { 
               ProvinceData = GeoSamplingData.filter(item => { return d.properties.ADM1_PCODE == item.PCODE; });
               var Val = ProvinceData.length != 0 ? parseInt(ProvinceData[0].Value) : 0;
               Tooltip = "<h6>" + d.properties.ADM1_EN + "</h6>" + Val; 
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





