module Abalone {
export module Frontend {

	export class LocalPlayer implements PlayerAgent {
		constructor(private renderer: Renderer) {}

		public play(g: Engine.Game, cb: (g: Engine.Game) => void): void {
		    var selectedPieces: Engine.Segment;

		    var disabled = false;
		    var dragInProgress = false;
		    var originHex = null;

		    var finish = (m: Engine.Move) => {
		        this.renderer.drawOverlay(null, false, g);
		        disabled = true;
		        var result = Engine.update(g, m);
		        console.log("renderer/play/finish:", result);
		        cb(result);
		    }

		    var dragstart = () => {
		        if (disabled) return;
		        originHex = this.renderer.hoveredHex();
		        if (selectedPieces != null) {
		            var move = generateMove(selectedPieces, originHex);
		            if (move != null && Engine.validMove(g, move)) {
		                finish(move);
		            } else {
		                selectedPieces = null;
		            }
		        } else {
		            if ((selectedPieces = Engine.getSegment(g, originHex)) != null) {
		                dragInProgress = true;
		                this.renderer.drawOverlay(selectedPieces, true, g);
		            }
		        }
		    }

		    var drag = () => {
		        if (disabled) return;
		        var currentHex = this.renderer.hoveredHex();
		        selectedPieces = Engine.getSegment(g, originHex, currentHex);
		        this.renderer.drawOverlay(selectedPieces, true, g);
		    }

		    var dragend = () => {
		        if (disabled) return;
		        var currentHex = this.renderer.hoveredHex();
		        selectedPieces = Engine.getSegment(g, originHex, currentHex);
		        this.renderer.drawOverlay(selectedPieces, false, g);
		    }

		    this.renderer.eventBox.call(
		        d3.behavior.drag()
		            .on("dragstart", dragstart)
		            .on("drag", drag)
		            .on("dragend", dragend)
		    )
		}
    }
}
}
