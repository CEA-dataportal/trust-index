/* 
const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
console.log(urlParams);
const country = urlParams.get('iso');
console.log(country); */

 // settings

const overviewURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTLurpBx5OU04mhO3C6586ht-5N2FTSBlFIwQITW0AqSo6uj6jCHTyAbDMIgJuGBq04PPNNuQ9ojbcB/pub?gid=361636222&single=true&output=csv&force=on";
const samplingURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTLurpBx5OU04mhO3C6586ht-5N2FTSBlFIwQITW0AqSo6uj6jCHTyAbDMIgJuGBq04PPNNuQ9ojbcB/pub?gid=110833577&single=true&output=csv&force=on";
const geosamplingURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTLurpBx5OU04mhO3C6586ht-5N2FTSBlFIwQITW0AqSo6uj6jCHTyAbDMIgJuGBq04PPNNuQ9ojbcB/pub?gid=1873376386&single=true&output=csv&force=on";
const chartCTI_url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTLurpBx5OU04mhO3C6586ht-5N2FTSBlFIwQITW0AqSo6uj6jCHTyAbDMIgJuGBq04PPNNuQ9ojbcB/pub?gid=1760705416&single=true&output=csv&force=on";
const chartGeo_url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTLurpBx5OU04mhO3C6586ht-5N2FTSBlFIwQITW0AqSo6uj6jCHTyAbDMIgJuGBq04PPNNuQ9ojbcB/pub?gid=1444352522&single=true&output=csv&force=on";


const country = "Zambia";

let CTI=[];
let sampling=[];
var Overall_Index = [];
var Value_Index = [];
var Comp_Index = [];

var Volunteers_Index = [];
var Beneficiaries_Index = [];
var NonBeneficiaries_Index = [];
var CTIdata = [];
var SamplingAge_label = [];
var SamplingAge_Data = [];
var GeoSamplingData = [];
var chartCTIData = [];
var chartGeoData = [];
var mapData ;
var totSampling ;
// for ColorScale
var colorScale ;

$(document).ready(function() {
    function getData() {
        Promise.all([
            d3.csv(overviewURL),
            d3.csv(samplingURL), // Sampling data
            d3.json("../data/ZMB.geojson"),
            d3.csv(geosamplingURL), // Sampling map
            d3.csv(chartCTI_url), // Chart data
            d3.csv(chartGeo_url), // Geo Chart data
        ]).then(function(data) {
            CTI=data[0];
            
            Overall_Index = [parseFloat(CTI[0]['Index']).toFixed(0)]; 
            Comp_Index = [parseFloat(CTI[0]['Competencies']).toFixed(0)]; 
            Value_Index = [parseFloat(CTI[0]['Values']).toFixed(0)]; 
            Volunteers_Index = [parseFloat(CTI[0]['Volunteers']).toFixed(0)];
            Beneficiaries_Index = [parseFloat(CTI[0]['Beneficiaries']).toFixed(0)];
            NonBeneficiaries_Index = [parseFloat(CTI[0]['Non-beneficiaries']).toFixed(0)];
            CTIdata = [parseFloat(CTI[0]['Non-beneficiaries']), parseFloat(CTI[0]['Beneficiaries']), parseFloat(CTI[0]['Volunteers'])];
            
             // Sampling data
            sampling=data[1];
            SamplingAge_label = [sampling[2]['Age1'], sampling[2]['Age2'], sampling[2]['Age3'], sampling[2]['Age4']];
            SamplingAge_Data = [parseFloat(sampling[1]['Age1']), parseFloat(sampling[1]['Age2']), parseFloat(sampling[1]['Age3']), parseFloat(sampling[1]['Age4'])];
            console.log(SamplingAge_Data);
            totSampling = sampling[0]['Total_respondent'];

             // Chart data
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

            // Geo Chart data
            chartGeoData = data[5];
            console.log(chartGeoData);
            var GeoDataComp = [];
            var GeoLabelComp= [];
            var GeoDataValue = [];
            var GeoLabelValue= [];
            chartGeoData.forEach(element => {
              if(element.Dimension == "Value"){
              GeoDataValue.push(element.Overall);
              GeoLabelValue.push(element.Geo);
              }
              if(element.Dimension == "Competency"){
                GeoDataComp.push(element.Overall);
                GeoLabelComp.push(element.Geo);
                }
             });
             console.log(GeoLabelComp);



             // Sampling map
            GeoSamplingData = data[3];
            GeoSamplingValue = [];
            GeoSamplingData.forEach(element => {
             GeoSamplingValue.push(parseInt(element.Value))
             });
            colorScale = d3.scaleLinear()
            .domain([0, d3.max(GeoSamplingValue)])
            .range(["#ffcccc", "#FF0000"]);

            mapData = data[2];
            // Text
            title(CTI);
            background(CTI);
            report(CTI);
            coverage(CTI);
            lead(CTI);
            date(CTI);
            
          // Chart
            generateRadialChart(Overall_Index);
            generateRadial_Chart1(Volunteers_Index);             
            generateRadial_Chart2(Beneficiaries_Index);
            generateRadial_Chart3(NonBeneficiaries_Index);
            generate_chartRadar1 (OverallComp, driversComp);
            generate_chartRadar2 (OverallValue, driversValue);
            generate_chart_geo_Comp (GeoDataComp, GeoLabelComp, Comp_Index);
            generate_chart_geo_Val (GeoDataValue, GeoLabelValue, Value_Index);
            // sampling
            figures(sampling);
            figFemales(sampling);
            figMales(sampling);
            limits(sampling);
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


 // Overall Chart 

function generateRadialChart(data){
    var optionsCircle4 = {
        chart: {
          type: 'radialBar',
          height: 350,
          width: '100%',
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


// Radar Chart Competencies
  
  function generate_chartRadar1 (OverallComp, driversComp){

  var optionsChartCTI = {
   
   colors: ['#FF0000'],
   series: [{
       name: 'Score' ,
       data: OverallComp,
     }
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

//  Radar Chart Values
function generate_chartRadar2 (OverallValue, driversValue){

var optionsRadar2 = {
 colors: ['#5383cc'],
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


      
// District charts 
  // COMPENTENCIES
function generate_chart_geo_Comp (GeoDataComp, GeoLabelComp, Comp_Index) {
  var optionsDistrict_Comp = {
  series: [{
  data: GeoDataComp,
  name: 'Score',
}],
  chart: {
  type: 'bar',
  height: 350
},
annotations: {
  xaxis: [{
    x: Comp_Index/10,
    borderColor: '#FF0000',
    label: {
      borderColor: '#FF0000',
      style: {
        color: '#fff',
        background: '#FF0000',
        fontSize: '14px',
      },
      text: 'Competencies Score',
    }
  }],
  yaxis: []
}, //end of annotation

colors: ['#18396C'],

plotOptions: {
  bar: {
    horizontal: true,
    distributed: false,
    rangeBarOverlap: true
  }
},//end of plotOption
dataLabels: {
  enabled: true,
  formatter: function (val){
    return parseFloat(val).toFixed(1)
        }
},
stroke: {
  width: 1,
  colors: ["#fff"]
},
xaxis: {
  categories: GeoLabelComp,
  labels: {
    show: true,
    style: {
     fontSize: '14px',
     color: '#CCC',
   },
    }
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
  reversed: false,
  axisTicks: {
    show: false
  },
  labels: {
    show: true,
    style: {
     fontSize: '14px',
   },
    }
},
};

var chartDistrict_Comp = new ApexCharts(document.querySelector("#chartDistrict_Comp"), optionsDistrict_Comp);
chartDistrict_Comp.render();

}


  // VALUES

function generate_chart_geo_Val (GeoDataValue, GeoLabelValue, Value_Index) {
  var optionsDistrict_Val = {
  series: [{
  data: GeoDataValue,
  name: 'Score',
}],
  chart: {
  type: 'bar',
  height: 350
},
annotations: {
  xaxis: [{
    x: Value_Index/10,
    borderColor: '#5383cc',
    label: {
      borderColor: '#5383cc',
      style: {
        color: '#fff',
        background: '#5383cc',
        fontSize: '14px',
      },
      text: 'Values Score',
    }
  }],
  yaxis: []
}, //end of annotation

colors: ['#18396C'],

plotOptions: {
  bar: {
    horizontal: true,
    distributed: false,
    rangeBarOverlap: true
  }
},//end of plotOption
dataLabels: {
  enabled: true,
  formatter: function (val){
    return parseFloat(val).toFixed(1)
        }
},
stroke: {
  width: 1,
  colors: ["#fff"]
},
xaxis: {
  categories: GeoLabelValue,
  labels: {
    show: true,
    style: {
     fontSize: '14px',
     color: '#CCC',
   },
    }
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
  reversed: false,
  axisTicks: {
    show: false
  },
  labels: {
    show: true,
    style: {
     fontSize: '14px',
   },
    }
},
};

var chartDistrict_Val = new ApexCharts(document.querySelector("#chartDistrict_Val"), optionsDistrict_Val);
chartDistrict_Val.render();

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
       .html(''+ coverage + ' provinces'); 
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
var svg = d3.select("#map_sampling").select("svg").attr("width", 350).attr("height",350),
    width = +svg.attr("width"),
    height = +svg.attr("height");

// Map and projection
var projection = d3.geoAitoff()
    .scale(1250)
    .translate([-455,-125])

svg.style("border","10px");


function map(data){

  console.log(data),
        svg.append("g")
            .selectAll("path")
            .data(data.features)
            .enter().append("path")
                .attr("fill", function(d) {

                 ProvinceData = GeoSamplingData.filter(item => { return d.properties.name == item.Name; });//mettre les PCODE
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
               ProvinceData = GeoSamplingData.filter(item => { return d.properties.name == item.Name; });
               var Val = ProvinceData.length != 0 ? parseInt(ProvinceData[0].Value) : 0;
               Tooltip = "<h6>" + d.properties.name + " Province</h6>" + Val; 
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





