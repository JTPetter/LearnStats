//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0
import "../qml/qml_components" as LS

Form 
{
	columns: 1
	
	
	
	Group
	{
	
				DropDown
		{
		name: "LSdescCentralOrSpread"
		label: qsTr("Statistics to explain")
		indexDefaultValue: 0
		values:
		[
		{label: qsTr("Central tendency"),		value: "LSdescCentralTendency"},
		{label: qsTr("Spread"),	value: "LSdescSpread"}
		]
		id: lsDescCentralOrSpread
		}
		
		
			
	RadioButtonGroup
	{
		columns:	3
		name:		"dataType"
		title:		qsTr("Data Input Type")
		id:			dataType
		
		
		
						RadioButton
		{
			value:		"dataRandom"
			label:		qsTr("Random Sample")
			id:			dataTypeA
			checked:	true
		}
		
				RadioButton
		{
			value:		"dataSequence"
			label:		qsTr("Enter sequence")
			id:			dataTypeB
		}

		RadioButton
		{
			value:		"dataVariable"
			label:		qsTr("Select variable")
			id:			dataTypeC
			enabled:	mainWindow.dataAvailable
		}



	}


	TextArea
	{
		title:		qsTr("Comma-separated Sequence of Observations")
		visible:	dataTypeB.checked
		height:		100
		name:     "dataSequenceInput"
		textType:	JASP.TextTypeSource
		separators:	[",",";","\n"]
	}


	Group
	{
		visible: dataTypeC.checked

		VariablesForm
		{
			preferredHeight:	150

			AvailableVariablesList
			{
				name:	"allVariables"
				title:	qsTr("Available")
			}

			AssignedVariablesList
			{
				name:				"selectedVariable"
				title:				qsTr("Selected")
				singleVariable:		true
				allowedColumns:		["ordinal", "scale"]

			}
		}
	}

	}
	
	
		Section
	{
	title: qsTr("Central Tendency Measures")
	visible: lsDescCentralOrSpread.currentValue == "LSdescCentralTendency"
	

		RadioButtonGroup
		{
		title:                                  qsTr("Select Central Tendency Measure")
		name:                                   "LSdescCT"

			RadioButton
			{
			name:                               "LSdescMean"
			label:                              qsTr("Show Mean")
			checked:                            true
			}

			RadioButton
			{
			name:                               "LSdescMedian"
			label:                              qsTr("Show Median")
			}
		
			RadioButton
			{
			name:                               "LSdescMode"
			label:                              qsTr("Show Mode")
			}
			
						RadioButton
			{
			name:                               "LSdescMMM"
			label:                              qsTr("Compare All")
			}
		}
		
			CheckBox{name: "LSdescExplanationC";
			label: qsTr("Show explanation");
			checked: true
					}
					
	}
	
	
	
			Section
	{
	title: qsTr("Measures of spread")
	visible: lsDescCentralOrSpread.currentValue == "LSdescSpread"
	

		RadioButtonGroup
		{
		title:                                  qsTr("Select measure of spread")
		name:                                   "LSdescSpread"

			RadioButton
			{
			name:                               "LSdescRange"
			label:                              qsTr("Show range")
			checked:                            true
			}

			RadioButton
			{
			name:                               "LSdescQR"
			label:                              qsTr("Show quartiles")
			}
		
			RadioButton
			{
			name:                               "LSdescVar"
			label:                              qsTr("Show variance")
			}
			
			RadioButton
			{
			name:                               "LSdescSD"
			label:                              qsTr("Show standard deviation")
			}
		}
		
			CheckBox{name: "LSdescExplanationS";
			label: qsTr("Show explanation");
			checked: true
					}
					
	}
	
	

}