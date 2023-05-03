
const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);

const country = urlParams.get('iso');

$(function(){
    $("#includedNav").load("nav.html");
  });



// settings
const dataURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbooW7TmLrMZ8QNc4IlGq4mKaZQflviQ1WNPzeMHLemb8Nl5QdsDQnR5TnWHeNOzsFY479CV-tHbNY/pub?gid=1401474139&single=true&output=csv&force=on";
const samplingURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbooW7TmLrMZ8QNc4IlGq4mKaZQflviQ1WNPzeMHLemb8Nl5QdsDQnR5TnWHeNOzsFY479CV-tHbNY/pub?gid=110833577&single=true&output=csv&force=on";

let CTI=[];
let sampling=[];
const countryISO = country;
var CTIdata = [];
var SamplingData = [];


$(document).ready(function() {
    function getData() {
        Promise.all([
            d3.csv(dataURL),
            d3.csv(samplingURL),
        ]).then(function(data) {
            CTI=data[0].filter(d => { return d.ISO == countryISO; });
            sampling=data[1].filter(d => { return d.ISO == countryISO; });
            CTIdata = [parseInt(CTI[0]['Competency']), parseInt(CTI[0]['Value']), parseInt(CTI[0]['Overall'])];
            SamplingData = [parseInt(sampling[0]['Total_respondent']), parseInt(sampling[0]['Female_respondent']), parseInt(sampling[0]['Male_respondent'])];
            // Overview 
            title(CTI);
            background(CTI);
            generateRadialChart(CTIdata);
            // sampling
            figures(CTI);
        }); // then
       
    } // getData
    getData();
});

 // Overview Text 

function background() {

    var desc = CTI[0]['Background'];
    console.log(desc);
          d3.select("#background").append("span")
        .text(desc); 


}; 

function title() {

    var title = CTI[0]['Country'];
          d3.select("#title_country").append("span")
        .text(title); 


}; 

 // Overview Radial Chart 




 function generateRadialChart(data){
    console.log(data);
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
        labels: ['Competency', 'Value', 'Trust Index'],
        colors: ['#0080ff', '#5F61B5', '#FF0000'],
        legend: {
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

 // Sampling charts
 // Age group
      
 var optionsDist = {
    series: [{
    name: 'Males',
    data: [14.69, 12.95, 16.96, 11.08
    ]
  },
  {
    name: 'Females',
    data: [12.28, 9.35, 15.49, 7.21
    ]
  }
  ],
    chart: {
    type: 'bar',
    height: 300,
    stacked: false
  },
  colors: ['#008FFB', '#FF4560'],
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
    }
  },
  yaxis: {
    min: 0,
    max: 20,
    title: {
      // text: 'Age',
    },
  },
  tooltip: {
    shared: false,
    x: {
      formatter: function (val) {
        return val
      }
    },
    y: {
      formatter: function (val) {
        return Math.abs(val) + "%"
      }
    }
  },
  title: {
    text: 'Respondents'
  },
  xaxis: {
    categories: ['45+', '35-44', '25-34', '18-24'
    ],
    title: {
      text: 'Percent'
    },
    labels: {
      formatter: function (val) {
        return Math.abs(Math.round(val)) + "%"
      }
    }
  },
  };

  var chartDist = new ApexCharts(document.querySelector("#chartDist"), optionsDist);
  chartDist.render();

 

   // sampling

function figures() {

    var respondents = CTI[0]['Total_respondent'];
    console.log(respondents);
          d3.select("#item-1").append("span")
        .text(respondents); 


};