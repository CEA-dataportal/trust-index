/* 
const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
console.log(urlParams);
const country = urlParams.get('iso');
console.log(country); */

 // settings
const dataURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbooW7TmLrMZ8QNc4IlGq4mKaZQflviQ1WNPzeMHLemb8Nl5QdsDQnR5TnWHeNOzsFY479CV-tHbNY/pub?gid=1401474139&single=true&output=csv&force=on";
const samplingURL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbooW7TmLrMZ8QNc4IlGq4mKaZQflviQ1WNPzeMHLemb8Nl5QdsDQnR5TnWHeNOzsFY479CV-tHbNY/pub?gid=110833577&single=true&output=csv&force=on";

let CTI=[];
let sampling=[];
const countryISO = "ARG";
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
            console.log(SamplingData);
            title(CTI);
            background(CTI); 
            generateRadialChart(CTIdata);
            background(CTI);

            // sampling
            figures(CTI);
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

// Pie charts
  var finding1 = {
    series: [0.275862069, 0.186206897, 0.231034483, 0.303448276, 0.00344827680],
    labels: ["Not all", "Not so much", "Mostly yes", "Yes completely","Don't know"],
    colors:['#bf0000', '#ff0000','#6B66B7','#090088','#d6d6d6'],

    chart: {
        type: 'donut',
    
        },
    dataLabels: {
        enabled: true
          },
    tooltip: {
        enabled: true,
        formatter: "%"
            },
    responsive: [{
        breakpoint: 480,
        options: {
            chart: {
                width: 100
            },
        legend: {
            position: 'bottom'
        } 
        }
    }],
  
  };

  var chart_finding1 = new ApexCharts(document.querySelector("#chartf1"), finding1);
  chart_finding1.render();


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
  
function figures() {

    var respondents = CTI[0]['Total_respondent'];
    console.log(respondents);
          d3.select("#item-1").append("span")
        .text(respondents); 


};
